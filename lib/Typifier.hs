{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Typifier where

import Control.Lens (use, (%=))
import Control.Lens.Operators ((.=))
import Control.Monad (replicateM)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState)
import Data.List (delete, nub, uncons)
import SyntaxTree (Definition (Definition), Expression (Application, Boolean, Conjunction, Difference, Equality, IfThenElse, LogicalNegation, Number, Product, Quotient, Smaller, Sum, Variable), PrettyPrintable (prettyPrint), Program, Stage (Core), VariableName)

data TypifierState = TypifierState TypeAssumptions VariableStream

data FType = FBool | FInteger | TypeVariable VariableName | FType :->: FType deriving (Eq, Show)

data QuantifiedType = QuantifiedType [VariableName] FType deriving (Eq, Show)

data TypeAssumption = SimpleAssumption VariableName FType | QuantifiedAssumption VariableName QuantifiedType deriving (Eq, Show)

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

{- Typifier for programs, according to an adaptation of KFPTS iterative typification -}
instance Typifiable (Program Core) FType where
  -- the type assumptions include an assumption for every predefined operator - for example: '*' :: Integer -> Integer -> Integer

  -- we start by grouping the definitions into mutually recursive sets of functions

  -- we typify these mutually recursive sets in topological order

  -- we check if we obtained a type for the main function, and if that type is consistent with F's assumptions (must be Bool or Integer)
  -- if it is, we return that type
  -- it it isn't, typification fails

  -- TODO: this must be implemented correctly, for now it just returns a default value to make the compiler pipeline work
  typifier _ = return FInteger

{- This assumes that defs are a mutually recursive set of function definitions
 - it also assumes that any type assumptions from externally dependended upon functions (besides the ones in the mutually recursive set defs) are already established
 -}
instance Typifiable [Definition Core] [TypeAssumption] where
  typifier defs = do
    -- at the beginning, create the most general assumptions for our mutually recursive functions and refine them recursively 
    let toFunctionName (Definition f _ _) = f
    let functionNames = map toFunctionName defs
    newVars <- replicateM (length defs) getNewTypeVar
    let entries = zip functionNames newVars
    let initAssumptions = map (\(f, v) -> QuantifiedAssumption f (QuantifiedType [v] (TypeVariable v))) entries
    typifier' initAssumptions functionNames
    where
      {- the recursion/iteration happens here - it is a fixed point iteration until the producedAssumptions are equivalent to the preAssumptions 
       - since iterative typification in this form undecidable, this might not terminate for some programs 
       -}
      typifier' preAssumptions funNames = do
        mapM_ addToTypeAssumptions preAssumptions
        producedTypes <- traverse typifier defs
        mapM_ removeFromTypeAssumptions preAssumptions
        let producedAssumptions = zipWith QuantifiedAssumption funNames producedTypes
        let assumptionsAreEquivalent = and $ zipWith areEquivalentAssumptions preAssumptions producedAssumptions
        if assumptionsAreEquivalent
          then return producedAssumptions
          else typifier' producedAssumptions funNames

{- This assumes that all necessary type assumptions to typify f are already established
 - in the case of a recursive function, the another typifier will establish this before calling
 - also in the case of a recursive function, the resulting type may not be the final type, iteration may be needed (see program typifier)
 -}
instance Typifiable (Definition Core) QuantifiedType where
  typifier (Definition _ params e) = do
    -- calculate new type variables for use in the params' type assumptions
    vars <- replicateM (length params) getNewTypeVar
    let newAssumptions = zipWith (\v a -> SimpleAssumption v (TypeVariable a)) params vars
    mapM_ addToTypeAssumptions newAssumptions
    (tau, eqs) <- typifier e
    let constructArrowType [] t = t; constructArrowType (v : vs) t = TypeVariable v :->: constructArrowType vs t
    let unsubstitutedFType = constructArrowType vars tau
    -- cleanup the added type assumptions for the variables
    mapM_ removeFromTypeAssumptions newAssumptions
    -- the returned type is the constructed, substituted arrow type quantified by the type variables we chose in the beginning
    let substitutedFType = substituteFromEqs unsubstitutedFType eqs
    return $ QuantifiedType (typeVariables substitutedFType) substitutedFType

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

-- checks if type assumptions are equivalent
areEquivalentAssumptions :: TypeAssumption -> TypeAssumption -> Bool
areEquivalentAssumptions (SimpleAssumption v t) (SimpleAssumption v' t') = v == v' && areEquivalentTypes t t'
areEquivalentAssumptions (SimpleAssumption v t) (QuantifiedAssumption v' (QuantifiedType vs t')) = null vs && v == v' && areEquivalentTypes t t'
areEquivalentAssumptions (QuantifiedAssumption v qt) (QuantifiedAssumption v' qt') = v == v' && areEquivalentQuantifiedTypes qt qt'
areEquivalentAssumptions (QuantifiedAssumption v' (QuantifiedType vs t')) (SimpleAssumption v t) = null vs && v == v' && areEquivalentTypes t t'

-- checks if quantified types are equivalent
areEquivalentQuantifiedTypes :: QuantifiedType -> QuantifiedType -> Bool
areEquivalentQuantifiedTypes (QuantifiedType vs t) (QuantifiedType vs' t') = case equalize (zip vs vs') t t' of
  Nothing -> False
  Just _ -> True

-- checks if types are same modulo variable renamings
areEquivalentTypes :: FType -> FType -> Bool
areEquivalentTypes a b = case equalize [] a b of Nothing -> False; (Just _) -> True

-- tries to make two types equals by performing renamings and, if successful, returns the mapping
equalize :: [(VariableName, VariableName)] -> FType -> FType -> Maybe [(VariableName, VariableName)]
equalize _ FInteger FInteger = Just []
equalize _ FInteger FBool = Nothing
equalize _ FInteger (TypeVariable _) = Nothing
equalize _ FInteger (_ :->: _) = Nothing
equalize _ FBool FBool = Just []
equalize _ FBool FInteger = Nothing
equalize _ FBool (TypeVariable _) = Nothing
equalize _ FBool (_ :->: _) = Nothing
equalize _ (TypeVariable _) FInteger = Nothing
equalize _ (TypeVariable _) FBool = Nothing
equalize mappings (TypeVariable t) (TypeVariable t')
  | (t, t') `elem` mappings = Just mappings
  | t `elem` map fst mappings || t' `elem` map snd mappings = Nothing
  | otherwise = Just ((t, t') : mappings)
equalize _ (TypeVariable _) (_ :->: _) = Nothing
equalize _ (_ :->: _) FInteger = Nothing
equalize _ (_ :->: _) FBool = Nothing
equalize _ (_ :->: _) (TypeVariable _) = Nothing
equalize mappings (t1 :->: t2) (t3 :->: t4) = do
  mappings' <- equalize mappings t1 t3
  equalize mappings' t2 t4

typeVariables :: FType -> [VariableName]
typeVariables FBool = []
typeVariables FInteger = []
typeVariables (TypeVariable t) = [t]
typeVariables (a :->: b) = typeVariables a ++ typeVariables b

lookupAssumption :: VariableName -> TypeAssumptions -> Maybe TypeAssumption
lookupAssumption _ [] = Nothing
lookupAssumption v (a : as)
  | matches a = Just a
  | otherwise = lookupAssumption v as
  where
    matches (SimpleAssumption v' _) = v == v'
    matches (QuantifiedAssumption v' _) = v == v'

addToTypeAssumptions :: TypeAssumption -> Typifier ()
addToTypeAssumptions a = assumptions %= (a :)

removeFromTypeAssumptions :: TypeAssumption -> Typifier ()
removeFromTypeAssumptions a = assumptions %= delete a

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

-- substitute type variables in type given by first argument by the definitions given by the type equations in the second argument
substituteFromEqs :: FType -> TypeEquations -> FType
substituteFromEqs = foldr substituteFromEq
  where
    substituteFromEq (TypeVariable x :=: tau') tau = substitute (TypeVariable x) tau' tau
    substituteFromEq _ tau = tau

throwError :: String -> Typifier a
throwError s = throwE $ "Error during typification: " ++ s