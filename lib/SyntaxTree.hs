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
  | Greater Expression Expression
  | Negated Expression
  | Difference Expression Expression
  | Sum Expression Expression
  | Quotient Expression Expression
  | Product Expression Expression
  | Application Expression Expression
  | VariableName VariableName
  | Number Integer
  | Boolean Bool

type VariableName = String