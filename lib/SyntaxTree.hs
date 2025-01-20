module SyntaxTree where

newtype Program = Program [Definition]

data Definition = Definition [VariableName] Expression

data LocalDefinition = LocalDefinition VariableName Expression

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

type VariableName = String