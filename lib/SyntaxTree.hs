module SyntaxTree where

import Data.List (nub)
import Data.List.Extra (delete)

newtype Program = Program [Definition] deriving (Eq, Read, Show)

data Definition = Definition VariableName [VariableName] Expression deriving (Eq, Read, Show)

data LocalDefinition = LocalDefinition VariableName Expression deriving (Eq, Read, Show)

data Expression
  = Let [LocalDefinition] Expression
  | IfThenElse Expression Expression Expression
  | Disjunction Expression Expression
  | Conjunction Expression Expression
  | LogicalNegation Expression
  | Smaller Expression Expression
  | Equality Expression Expression
  | Minus Expression
  | Difference Expression Expression
  | Sum Expression Expression
  | Quotient Expression Expression
  | Product Expression Expression
  | Application Expression Expression
  | Variable VariableName
  | Number Integer
  | Boolean Bool
  deriving (Eq, Read, Show)

type VariableName = String

boundVariables :: Expression -> [VariableName]
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

freeVariables :: Expression -> [VariableName]
freeVariables (Let [] e) = freeVariables e
freeVariables (Let ((LocalDefinition v e) : defs) e') = delete v $ nub $ freeVariables e ++ freeVariables (Let defs e')
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
rename :: String -> String -> Expression -> Expression
rename v v' = substitute v (Variable v')

-- substitute any occurence of the free variable v in the expression given by the third parameter by the expression given by the second parameter
substitute :: VariableName -> Expression -> Expression -> Expression
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

substituteDefs :: [LocalDefinition] -> VariableName -> Expression -> [LocalDefinition]
substituteDefs [] _ _ = []
substituteDefs ((LocalDefinition v e) : defs) v' e' = LocalDefinition v (substitute v' e' e) : substituteDefs defs v' e'

isIndependentFrom :: LocalDefinition -> [LocalDefinition] -> Bool
isIndependentFrom def = not . any (isDependentOn def)

isDependentOn :: LocalDefinition -> LocalDefinition -> Bool
isDependentOn (LocalDefinition _ e) (LocalDefinition v _) = v `elem` freeVariables e