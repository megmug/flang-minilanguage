module MachineInstruction where

type FunctionName = String

type CodeAddress = Int

type HeapAddress = Int

type StackAddress = Int

type Arity = Int

data UpdateArg = PredefinedOperator | Arity Int deriving (Eq, Show, Read)

data FOperator
  = Smaller
  | Minus
  | FIf
  deriving (Eq, Show, Read)

data Instruction
  = Pushfun FunctionName
  | Pushval Integer
  | Pushparam Int
  | Makeapp
  | Slide Int
  | Unwind
  | Call
  | Return
  | Pushpre FOperator
  | Update UpdateArg
  | Operator FOperator
  | Halt
  deriving (Eq, Show, Read)