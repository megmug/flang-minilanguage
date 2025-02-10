{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module SyntaxTree where

import Data.List (nub, permutations, subsequences, (\\))

{- The stage s is parameterizing an expression to indicate if it is a raw program (directly after parsing), or an already rewritten one
 - This allows simplifying the typifier and code generator, since it essentially hides the unneeded constructers in later compilation stages
 -}
data Stage = Raw | Core

newtype Program (s :: Stage) = Program [Definition s] deriving (Eq, Show)

data Definition (s :: Stage) = Definition VariableName [VariableName] (Expression s) deriving (Eq, Show)

data LocalDefinition = LocalDefinition VariableName (Expression Raw) deriving (Eq, Show)

data Expression (s :: Stage) where
  IfThenElse :: Expression s -> Expression s -> Expression s -> Expression s
  Conjunction :: Expression s -> Expression s -> Expression s
  LogicalNegation :: Expression s -> Expression s
  Smaller :: Expression s -> Expression s -> Expression s
  Equality :: Expression s -> Expression s -> Expression s
  Difference :: Expression s -> Expression s -> Expression s
  Sum :: Expression s -> Expression s -> Expression s
  Quotient :: Expression s -> Expression s -> Expression s
  Product :: Expression s -> Expression s -> Expression s
  Application :: Expression s -> Expression s -> Expression s
  Variable :: VariableName -> Expression s
  Number :: Integer -> Expression s
  Boolean :: Bool -> Expression s
  -- These constructors exist only in a Raw Expression (before rewriting):
  Let :: [LocalDefinition] -> Expression Raw -> Expression Raw
  Disjunction :: Expression Raw -> Expression Raw -> Expression Raw
  Minus :: Expression Raw -> Expression Raw

deriving instance Show (Expression s)

deriving instance Eq (Expression s)

type VariableName = String

-- This is just an embedding from Core to Raw Expressions that is unfortunately needed for some operations on Expressions
coreToRaw :: Expression Core -> Expression Raw
coreToRaw (IfThenElse a b c) = IfThenElse (coreToRaw a) (coreToRaw b) (coreToRaw c)
coreToRaw (Conjunction a b) = Conjunction (coreToRaw a) (coreToRaw b)
coreToRaw (LogicalNegation a) = LogicalNegation (coreToRaw a)
coreToRaw (Smaller a b) = Smaller (coreToRaw a) (coreToRaw b)
coreToRaw (Equality a b) = Equality (coreToRaw a) (coreToRaw b)
coreToRaw (Difference a b) = Difference (coreToRaw a) (coreToRaw b)
coreToRaw (Sum a b) = Sum (coreToRaw a) (coreToRaw b)
coreToRaw (Quotient a b) = Quotient (coreToRaw a) (coreToRaw b)
coreToRaw (Product a b) = Product (coreToRaw a) (coreToRaw b)
coreToRaw (Application a b) = Application (coreToRaw a) (coreToRaw b)
coreToRaw (Variable name) = Variable name
coreToRaw (Number n) = Number n
coreToRaw (Boolean b) = Boolean b

class PrettyPrintable t where
  prettyPrint :: t -> String

instance PrettyPrintable (Program s) where
  prettyPrint (Program defs) = prettyPrint defs

instance PrettyPrintable [Definition s] where
  prettyPrint [] = ""
  prettyPrint (def : defs) = prettyPrint def ++ if null defs then "" else " " ++ prettyPrint defs

instance PrettyPrintable (Definition s) where
  prettyPrint (Definition f params e) = f ++ prettyPrintVars params ++ " = " ++ prettyPrint e ++ ";"
    where
      prettyPrintVars [] = ""
      prettyPrintVars (p : ps) = " " ++ p ++ prettyPrintVars ps

instance PrettyPrintable (Expression s) where
  prettyPrint (Let defs e) = "let " ++ prettyPrint defs ++ " in " ++ prettyPrint e
  prettyPrint (IfThenElse cond e1 e2) = "if " ++ prettyPrint cond ++ " then " ++ prettyPrint e1 ++ " else " ++ prettyPrint e2
  prettyPrint (Disjunction e1 e2) = "(" ++ prettyPrint e1 ++ ") | (" ++ prettyPrint e2 ++ ")"
  prettyPrint (Conjunction e1 e2) = "(" ++ prettyPrint e1 ++ ") & (" ++ prettyPrint e2 ++ ")"
  prettyPrint (LogicalNegation e) = "not " ++ "(" ++ prettyPrint e ++ ")"
  prettyPrint (Smaller e1 e2) = "(" ++ prettyPrint e1 ++ ") < (" ++ prettyPrint e2 ++ ")"
  prettyPrint (Equality e1 e2) = "(" ++ prettyPrint e1 ++ ") == (" ++ prettyPrint e2 ++ ")"
  prettyPrint (Minus e) = "-(" ++ prettyPrint e ++ ")"
  prettyPrint (Difference e1 e2) = "(" ++ prettyPrint e1 ++ ") - (" ++ prettyPrint e2 ++ ")"
  prettyPrint (Sum e1 e2) = "(" ++ prettyPrint e1 ++ ") + (" ++ prettyPrint e2 ++ ")"
  prettyPrint (Quotient e1 e2) = "(" ++ prettyPrint e1 ++ ") / (" ++ prettyPrint e2 ++ ")"
  prettyPrint (Product e1 e2) = "(" ++ prettyPrint e1 ++ ") * (" ++ prettyPrint e2 ++ ")"
  prettyPrint (Application e1 e2) = "(" ++ prettyPrint e1 ++ ") (" ++ prettyPrint e2 ++ ")"
  prettyPrint (Variable v) = v
  prettyPrint (Number n) = show n
  prettyPrint (Boolean b) = if b then "true" else "false"

instance PrettyPrintable [LocalDefinition] where
  prettyPrint [] = ""
  prettyPrint (def : defs) = prettyPrint def ++ if null defs then "" else "; " ++ prettyPrint defs

instance PrettyPrintable LocalDefinition where
  prettyPrint (LocalDefinition v e) = v ++ " = " ++ prettyPrint e

boundVariables :: Expression s -> [VariableName]
boundVariables (Let [] e) = boundVariables e
boundVariables (Let ((LocalDefinition v e) : defs) e') = nub $ [v] ++ boundVariables e ++ boundVariables (Let defs e')
boundVariables (Disjunction e1 e2) = nub $ boundVariables e1 ++ boundVariables e2
boundVariables (Conjunction e1 e2) = nub $ boundVariables e1 ++ boundVariables e2
boundVariables (LogicalNegation e) = boundVariables e
boundVariables (Smaller e1 e2) = nub $ boundVariables e1 ++ boundVariables e2
boundVariables (Equality e1 e2) = nub $ boundVariables e1 ++ boundVariables e2
boundVariables (Minus e) = boundVariables e
boundVariables (Difference e1 e2) = nub $ boundVariables e1 ++ boundVariables e2
boundVariables (Sum e1 e2) = nub $ boundVariables e1 ++ boundVariables e2
boundVariables (Quotient e1 e2) = nub $ boundVariables e1 ++ boundVariables e2
boundVariables (Product e1 e2) = nub $ boundVariables e1 ++ boundVariables e2
boundVariables (Application e1 e2) = nub $ boundVariables e1 ++ boundVariables e2
-- numbers, booleans and variables don't bind anything
boundVariables _ = []

freeVariables :: Expression s -> [VariableName]
freeVariables (Let [] e) = freeVariables e
freeVariables (Let (def@(LocalDefinition _ e) : defs) e') = nub (freeVariables e ++ freeVariables (Let defs e')) \\ boundByTopLevelVars (def : defs)
freeVariables (IfThenElse e1 e2 e3) = nub $ freeVariables e1 ++ freeVariables e2 ++ freeVariables e3
freeVariables (Disjunction e1 e2) = nub $ freeVariables e1 ++ freeVariables e2
freeVariables (Conjunction e1 e2) = nub $ freeVariables e1 ++ freeVariables e2
freeVariables (LogicalNegation e) = freeVariables e
freeVariables (Smaller e1 e2) = nub $ freeVariables e1 ++ freeVariables e2
freeVariables (Equality e1 e2) = nub $ freeVariables e1 ++ freeVariables e2
freeVariables (Minus e) = nub $ freeVariables e
freeVariables (Difference e1 e2) = nub $ freeVariables e1 ++ freeVariables e2
freeVariables (Sum e1 e2) = nub $ freeVariables e1 ++ freeVariables e2
freeVariables (Quotient e1 e2) = nub $ freeVariables e1 ++ freeVariables e2
freeVariables (Product e1 e2) = nub $ freeVariables e1 ++ freeVariables e2
freeVariables (Application e1 e2) = nub $ freeVariables e1 ++ freeVariables e2
freeVariables (Variable v) = [v]
-- numbers and booleans are not free variables
freeVariables _ = []

-- rename free variable (first parameter) in the given expression to the string given by the second parameter
rename :: String -> String -> Expression s -> Expression s
rename v v' = substitute v (Variable v')

-- substitute any occurence of the free variable v in the expression given by the third parameter by the expression given by the second parameter
substitute :: VariableName -> Expression s -> Expression s -> Expression s
substitute v e' (Let defs e) = Let substitutedDefs substitutedE
  where
    substitutedDefs = map substituteDef defs
    substituteDef (LocalDefinition v' expr) =
      if v' == v
        then LocalDefinition v expr
        else LocalDefinition v (substitute v e' expr)
    substitutedE = if boundByTopLevel v defs then e else substitute v e' e
substitute v e (IfThenElse e1 e2 e3) = IfThenElse (substitute v e e1) (substitute v e e2) (substitute v e e3)
substitute v e (Disjunction e1 e2) = Disjunction (substitute v e e1) (substitute v e e2)
substitute v e (Conjunction e1 e2) = Conjunction (substitute v e e1) (substitute v e e2)
substitute v e' (LogicalNegation e) = LogicalNegation $ substitute v e' e
substitute v e (Smaller e1 e2) = Smaller (substitute v e e1) (substitute v e e2)
substitute v e (Equality e1 e2) = Equality (substitute v e e1) (substitute v e e2)
substitute v e' (Minus e) = Minus $ substitute v e' e
substitute v e (Difference e1 e2) = Difference (substitute v e e1) (substitute v e e2)
substitute v e (Sum e1 e2) = Sum (substitute v e e1) (substitute v e e2)
substitute v e (Quotient e1 e2) = Quotient (substitute v e e1) (substitute v e e2)
substitute v e (Application e1 e2) = Application (substitute v e e1) (substitute v e e2)
substitute v e (Product e1 e2) = Product (substitute v e e1) (substitute v e e2)
substitute v' e' e@(Variable v) = if v == v' then e' else e
substitute _ _ e@(Number _) = e
substitute _ _ e@(Boolean _) = e

boundByTopLevel :: VariableName -> [LocalDefinition] -> Bool
boundByTopLevel _ [] = False
boundByTopLevel v ((LocalDefinition v' _) : definitions) = v == v' || boundByTopLevel v definitions

boundByTopLevelVars :: [LocalDefinition] -> [VariableName]
boundByTopLevelVars [] = []
boundByTopLevelVars ((LocalDefinition v _) : defs) = v : boundByTopLevelVars defs

substituteDefs :: [LocalDefinition] -> VariableName -> Expression Raw -> [LocalDefinition]
substituteDefs [] _ _ = []
substituteDefs ((LocalDefinition v e) : defs) v' e' = LocalDefinition v (substitute v' e' e) : substituteDefs defs v' e'

hasConflictingLetBindings :: [LocalDefinition] -> Bool
hasConflictingLetBindings xs = projectVars /= nub projectVars
  where
    leftSide (LocalDefinition v _) = v
    projectVars = map leftSide xs

isIndependentFrom :: LocalDefinition -> [LocalDefinition] -> Bool
isIndependentFrom def = not . any (isDependentOn def)

isDependentOn :: LocalDefinition -> LocalDefinition -> Bool
isDependentOn (LocalDefinition _ e) (LocalDefinition v _) = v `elem` freeVariables e

extendRelation :: (a -> a -> Bool) -> a -> a -> [a] -> Bool
extendRelation r a b [] = a `r` b
extendRelation r a b (x : xs) = (a `r` x) && extendRelation r x b xs

transitiveExtension :: (a -> a -> Bool) -> [a] -> (a -> a -> Bool)
transitiveExtension r xs a b = let allOrderedSubsequences = concatMap permutations (subsequences xs) in any (extendRelation r a b) allOrderedSubsequences

{- perform a topological sort of the local definitions (in a let binding) failing if the bindings are recursive -}
topologicallySort :: [LocalDefinition] -> Either String [LocalDefinition]
topologicallySort [] = Right []
topologicallySort (x : xs)
  | transitiveExtension isDependentOn (x : xs) x x = Left $ "local definitions " ++ prettyPrint (x : xs) ++ " are recursive!"
  | x `isIndependentFrom` xs = do
      sortedDefs <- topologicallySort xs
      return $ x : sortedDefs
  | otherwise = topologicallySort $ xs ++ [x]