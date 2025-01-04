module MachineInstruction where

type FunctionName = String

type CodeAddress = Int

type HeapAddress = Int

type StackAddress = Int

type StackElement = Integer

data FType = FInteger | FBool deriving (Eq, Show, Read)

type Arity = Int

data UpdateArg = PredefinedOperator | Arity Int deriving (Eq, Show, Read)

data OperatorArg = One | Two | OpIf deriving (Eq, Show, Read)

data FOperator
  = Equals
  | Smaller
  | Plus
  | Minus
  | Times
  | Divide
  | And
  | Or
  | Not
  | FIf
  deriving (Eq, Show, Read)

data Instruction
  = Reset
  | AddDef FunctionName Arity CodeAddress
  | Pushfun FunctionName
  | Pushval FType Integer
  | Pushparam Int
  | Makeapp
  | Slide Int
  | Reduce
  | Unwind
  | Call
  | Return
  | Pushpre FOperator
  | Update UpdateArg
  | Operator OperatorArg
  | Halt
  deriving (Eq, Show, Read)