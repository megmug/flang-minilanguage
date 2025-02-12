{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Typifier where

import Control.Lens (use)
import Control.Lens.Operators ((.=))
import Control.Monad (replicateM)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState)
import Data.List (nub, uncons)
import SyntaxTree (Expression (Application, Boolean, Conjunction, Difference, Equality, IfThenElse, LogicalNegation, Number, Product, Quotient, Smaller, Sum, Variable), PrettyPrintable (prettyPrint), Program, Stage (Core), VariableName)

data TypifierState = TypifierState TypeAssumptions VariableStream

data FType = FBool | FInteger | TypeVariable VariableName | FType :->: FType deriving (Eq, Show)

data QuantifiedType = QuantifiedType [VariableName] FType

data TypeAssumption = SimpleAssumption VariableName FType | QuantifiedAssumption VariableName QuantifiedType

type TypeAssumptions = [TypeAssumption]

data TypeEquation = FType :=: FType deriving (Eq, Show)

type TypeEquations = [TypeEquation]

type VariableStream = [String]

type Typifier a = ExceptT String (State TypifierState) a

{- Lens definitions for TypifierState -}
assumptions :: (Functor f) => (TypeAssumptions -> f TypeAssumptions) -> TypifierState -> f TypifierState
assumptions f (TypifierState ass vars) = (\assumptions' -> TypifierState assumptions' vars) <$> f ass

varStream :: (Functor f) => (VariableStream -> f VariableStream) -> TypifierState -> f TypifierState
varStream f (TypifierState ass vars) = (\varStream' -> TypifierState ass varStream') <$> f vars

{- Type class for syntactical elements that can be typified -}
class Typifiable a b where
  -- A typifier creates a monadic action from a syntactical element that can generate code for it
  typifier :: a -> Typifier b

  -- This runs a typifier with some supplied state (can also be useful for testing)
  customTypify :: a -> TypifierState -> Either String b
  customTypify e = evalState (runExceptT $ typifier e)

  -- This runs a typifier with some default empty state (mostly useful for whole programs)
  typify :: a -> Either String b
  typify e = customTypify e $ TypifierState [] ["_t" ++ show i | i <- [0 ..] :: [Integer]]

{- Rough outline for how we want to typify a program:
 - we start by grouping the definitions into mutually recursive sets of functions
 - we sort the set of these sets topologically
 - we typify these mutually recursive sets in topological order
 - to kickstart the typification process of such a set, we calculate type assumptions based on the function signatures
 - the type assumptions also include an assumption for every predefined operator - for example: '*' :: Integer -> Integer -> Integer
 - we iteratively typify again and again, obtaining new type assumptions every time, until the assumptions stabilize (this should be roughly the iterative typification in KFPTS)
 - once we obtain a (stable) type for the main function, we stop
 - if the obtained main type is a bool or integer, typification is successful and we return that type
 - if not, typification fails
 - also, typification can fail at any point if any inconsistent type is encountered (e.g. * instantiated with a bool
 -}
instance Typifiable (Program Core) FType where
  -- TODO: this must be implemented correctly, for now it just returns a default value to make the compiler pipeline work
  typifier _ = return FInteger

instance Typifiable (Expression Core) (FType, TypeEquations) where
  typifier e@(IfThenElse cond e1 e2) = do
    (tauCond, eqsCond) <- typifier cond
    (tau1, eqs1) <- typifier e1
    (tau2, eqs2) <- typifier e2
    v <- getNewTypeVar
    let alpha = TypeVariable v
    let newEqs = nub $ eqsCond ++ eqs1 ++ eqs2 ++ [alpha :=: tau1, alpha :=: tau2, tauCond :=: FBool]
    case unify newEqs of
      Nothing -> throwError $ "error unifying type equations " ++ show newEqs ++ " for expression " ++ prettyPrint e
      Just newUnifiedEqs -> return (alpha, newUnifiedEqs)
  typifier (Conjunction e1 e2) = typifier (Application (Application (Variable "&") e1) e2)
  typifier (LogicalNegation e) = typifier (Application (Variable "¬") e)
  typifier (Smaller e1 e2) = typifier (Application (Application (Variable "<") e1) e2)
  typifier (Equality e1 e2) = typifier (Application (Application (Variable "=") e1) e2)
  typifier (Difference e1 e2) = typifier (Application (Application (Variable "-") e1) e2)
  typifier (Sum e1 e2) = typifier (Application (Application (Variable "+") e1) e2)
  typifier (Quotient e1 e2) = typifier (Application (Application (Variable "/") e1) e2)
  typifier (Product e1 e2) = typifier (Application (Application (Variable "*") e1) e2)
  typifier e@(Application e1 e2) = do
    (tau1, eqs1) <- typifier e1
    (tau2, eqs2) <- typifier e2
    alpha <- getNewTypeVar
    let newTypeEq = tau1 :=: (tau2 :->: TypeVariable alpha)
    let newEqs = nub $ eqs1 ++ eqs2 ++ [newTypeEq]
    case unify newEqs of
      Nothing -> throwError $ "error unifying type equations " ++ show newEqs ++ " for expression " ++ prettyPrint e
      Just newUnifiedEqs -> return (TypeVariable alpha, newUnifiedEqs)
  typifier (Variable v) = do
    ass <- use assumptions
    case lookupAssumption v ass of
      Nothing -> throwError $ "found no type assumption for variable " ++ v
      {- in this case, we are typifying a simple variable that is not a global function / supercombinator
       - we can just use the stored type assumption directly -}
      Just (SimpleAssumption _ t) -> return (t, [])
      -- in this case, we are typifying a supercombinator-instantiation so we need to instantiate the quantified assumption into a new polymorphic type using new type variables
      Just (QuantifiedAssumption _ (QuantifiedType quantifiedVars t)) -> do
        -- generate as many new variables as needed
        let numVars = length quantifiedVars
        newVars <- replicateM numVars getNewTypeVar
        let substitutions = zip (map TypeVariable quantifiedVars) (map TypeVariable newVars)
        -- apply all substitutions in order
        let subsitutedType = foldr (uncurry substitute) t substitutions
        return (subsitutedType, [])
  typifier (Number _) = return (FInteger, [])
  typifier (Boolean _) = return (FBool, [])

lookupAssumption :: VariableName -> TypeAssumptions -> Maybe TypeAssumption
lookupAssumption _ [] = Nothing
lookupAssumption v (a : as)
  | matches a = Just a
  | otherwise = lookupAssumption v as
  where
    matches (SimpleAssumption v' _) = v == v'
    matches (QuantifiedAssumption v' _) = v == v'

getNewTypeVar :: Typifier VariableName
getNewTypeVar = do
  vars <- use varStream
  case uncons vars of
    Nothing -> throwError "need a new variable, but got none left!"
    Just (v, vars') -> do
      varStream .= vars'
      return v

unify :: TypeEquations -> Maybe TypeEquations
unify = unificationStep' 0
  where
    unificationStep' i xs =
      if 0 <= i && i < length xs
        then do
          xs' <- unificationStepAt i xs
          if xs' /= xs
            then do
              unificationStep' 0 xs'
            else do
              unificationStep' (i + 1) xs'
        else return xs
    unificationStepAt i xs =
      if 0 <= i && i < length xs
        then do
          case splitAt i xs of
            (xs', []) -> return xs'
            (xs', y : zs') -> do
              let solvedXs = applySolveUnificationRule y xs'
              let solvedZs = applySolveUnificationRule y zs'
              ys <- applySimpleUnificationRule y
              return (solvedXs ++ ys ++ solvedZs)
        else return xs

{- Apply a simple (i.e. context-independent) unification rule -}
applySimpleUnificationRule :: TypeEquation -> Maybe TypeEquations
-- FAIL1: Two different type constructors cannot be equal!
applySimpleUnificationRule (FBool :=: FInteger) = Nothing
applySimpleUnificationRule (FInteger :=: FBool) = Nothing
-- FAIL2: A type constructor and arrow type cannot be equal! (left side)
applySimpleUnificationRule (FBool :=: (_ :->: _)) = Nothing
applySimpleUnificationRule (FInteger :=: (_ :->: _)) = Nothing
-- FAIL3: A type constructor and arrow type cannot be equal! (right side)
applySimpleUnificationRule ((_ :->: _) :=: FBool) = Nothing
applySimpleUnificationRule ((_ :->: _) :=: FInteger) = Nothing
-- DECOMPOSE: Two arow types are decomposed into separate equations for left and right side
applySimpleUnificationRule ((a :->: b) :=: (c :->: d)) = Just [a :=: c, b :=: d]
-- ORIENT: An equation where the right side is a variable, and the left side is not, can be flipped
applySimpleUnificationRule (a :=: (TypeVariable b)) | not $ isTypeVariable a = Just [TypeVariable b :=: a]
-- ELIM: If both sides are equal, the equation is redundant
applySimpleUnificationRule (a :=: b) | a == b = Just []
-- OCCURSCHECK: If left side is a type variable, right side is not, and left side occurs in right side, the equation is unsolvable (ill-defined)
applySimpleUnificationRule (a'@(TypeVariable _) :=: b) | b /= a' && (a' `occursIn` b) = Nothing
-- Otherwise, none of the simple rules match, so we return without any changes
applySimpleUnificationRule eq = Just [eq]

applySolveUnificationRule :: TypeEquation -> TypeEquations -> TypeEquations
applySolveUnificationRule (a@(TypeVariable _) :=: b) eqs | not $ a `occursIn` b = map (substituteEq a b) eqs
applySolveUnificationRule _ eqs = eqs

isTypeVariable :: FType -> Bool
isTypeVariable (TypeVariable _) = True
isTypeVariable _ = False

-- check if left side occurs in right side
occursIn :: FType -> FType -> Bool
occursIn a b | a == b = True
occursIn a (b :->: c) = a `occursIn` b || a `occursIn` c
occursIn _ _ = False

-- substitute type given by first argument by type given by second argument in third argument
substitute :: FType -> FType -> FType -> FType
substitute a b c | a == c = b
substitute a b (c :->: d) = substitute a b c :->: substitute a b d
substitute _ _ c = c

-- substitute type given by first argument by type given by second argument in both sides of third argument
substituteEq :: FType -> FType -> TypeEquation -> TypeEquation
substituteEq a b (c :=: d) = substitute a b c :=: substitute a b d

throwError :: String -> Typifier a
throwError s = throwE $ "Error during typification: " ++ s