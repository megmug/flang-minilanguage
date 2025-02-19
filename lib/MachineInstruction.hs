module MachineInstruction where

type FunctionName = String

type CodeAddress = Int

type HeapAddress = Int

type StackAddress = Int

type Arity = Int

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
  | Unwind
  | Call
  | Return
  | Update Arity
  | Operator FOperator
  | Halt
  deriving (Eq, Show, Read)