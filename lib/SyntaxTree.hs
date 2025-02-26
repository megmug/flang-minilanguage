{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module SyntaxTree where

import Data.List (nub, (\\))
import GeneralLib (PrettyPrintable (prettyPrint))

{- The stage s is parameterizing an expression to indicate if it is a raw program (directly after parsing), or an already rewritten one
 - This allows simplifying the typifier and code generator, since it essentially hides the unneeded constructers in later compilation stages
 -}
data Stage = Raw | Core

newtype Program (s :: Stage) = Program [Definition s] deriving (Eq, Show)

data Definition (s :: Stage) = Definition VariableName [VariableName] (Expression s) deriving (Eq, Show)

data LocalDefinition = LocalDefinition VariableName (Expression Raw) deriving (Eq, Show)

data Expression (s :: Stage) where
  IfThenElse :: Expression s -> Expression s -> Expression s -> Expression s
  Smaller :: Expression s -> Expression s -> Expression s
  Difference :: Expression s -> Expression s -> Expression s
  Application :: Expression s -> Expression s -> Expression s
  Variable :: VariableName -> Expression s
  Number :: Integer -> Expression s
  -- These constructors exist only in a Raw Expression (before rewriting):
  Conjunction :: Expression Raw -> Expression Raw -> Expression Raw
  Disjunction :: Expression Raw -> Expression Raw -> Expression Raw
  LogicalNegation :: Expression Raw -> Expression Raw
  Equality :: Expression Raw -> Expression Raw -> Expression Raw
  Sum :: Expression Raw -> Expression Raw -> Expression Raw
  Minus :: Expression Raw -> Expression Raw
  Quotient :: Expression Raw -> Expression Raw -> Expression Raw
  Product :: Expression Raw -> Expression Raw -> Expression Raw
  Let :: [LocalDefinition] -> Expression Raw -> Expression Raw
  Boolean :: Bool -> Expression Raw

deriving instance Show (Expression s)

deriving instance Eq (Expression s)

type VariableName = String

-- This is just an embedding from Core to Raw Expressions that is unfortunately needed for some operations on Expressions
coreToRaw :: Expression Core -> Expression Raw
coreToRaw (IfThenElse cond e1 e2) = IfThenElse (coreToRaw cond) (coreToRaw e1) (coreToRaw e2)
coreToRaw (Smaller a b) = Smaller (coreToRaw a) (coreToRaw b)
coreToRaw (Difference a b) = Difference (coreToRaw a) (coreToRaw b)
coreToRaw (Application a b) = Application (coreToRaw a) (coreToRaw b)
coreToRaw (Variable name) = Variable name
coreToRaw (Number n) = Number n

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

boundVariablesInDef :: Definition s -> [VariableName]
boundVariablesInDef (Definition f params e) = nub $ (f : params) ++ boundVariables e

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

freeVariablesInDef :: Definition s -> [VariableName]
freeVariablesInDef (Definition f params e) = freeVariables e \\ (f : params)

isClosedDefinition :: Definition s -> [Definition s] -> Bool
isClosedDefinition def defs = all (`appearsIn` defs) (freeVariablesInDef def)
  where
    appearsIn _ [] = False
    appearsIn g (Definition f' _ _ : defs') = g == f' || g `appearsIn` defs'

isClosedProgram :: Program s -> Bool
isClosedProgram (Program defs) = all (`isClosedDefinition` defs) defs

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

isDependentOn :: LocalDefinition -> LocalDefinition -> Bool
isDependentOn (LocalDefinition _ e) (LocalDefinition v _) = v `elem` freeVariables e