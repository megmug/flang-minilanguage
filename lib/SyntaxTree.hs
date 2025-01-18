module SyntaxTree where

type Program = [Definition]

data Definition = Definition [VariableName] Expression

data LocalDefinition = LocalDefinition VariableName Expression

data Expression
  = DisjunctionExpression DisjunctionExpression
  | LetExpression [LocalDefinition] Expression
  | IfThenElseExpression Expression Expression Expression

data DisjunctionExpression
  = Disjunction ConjunctionExpression DisjunctionExpression
  | TrivialDisjunction ConjunctionExpression

data ConjunctionExpression
  = Conjunction NegationExpression ConjunctionExpression
  | TrivialConjunction NegationExpression

data NegationExpression
  = LogicalNegative SmallerEqualsExpression
  | LogicalPositive SmallerEqualsExpression

data SmallerEqualsExpression
  = Smaller UnaryMinusExpression SmallerEqualsExpression
  | Equals UnaryMinusExpression SmallerEqualsExpression
  | TrivialSmallerEquals UnaryMinusExpression

data UnaryMinusExpression
  = Minus BinaryMinusExpression
  | Plus BinaryMinusExpression

data BinaryMinusExpression
  = BinaryMinus AdditionExpression BinaryMinusExpression
  | TrivialMinus AdditionExpression

data AdditionExpression
  = Sum DivisionExpression AdditionExpression
  | TrivialSum DivisionExpression

data DivisionExpression
  = Division MultiplicationExpression DivisionExpression
  | TrivialDivision MultiplicationExpression

data MultiplicationExpression
  = Multiplication ApplicationExpression MultiplicationExpression
  | TrivialMultiplication ApplicationExpression

data ApplicationExpression = Application Expression [Expression]

data AtomicExpression
  = VariableName VariableName
  | Number Integer
  | Boolean Bool

type VariableName = String