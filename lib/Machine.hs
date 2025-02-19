{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use record patterns" #-}
module Machine where

import Control.Lens (use, (.=))
import Control.Lens.Operators ((+=))
import Control.Monad (replicateM_)
import Control.Monad.Extra (whileM)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State (StateT)
import Data.IntMap as M (IntMap, adjust, filter, fromList, insert, lookup, lookupMin)
import Data.List.Index as I (indexed)
import Data.Vector as V (Vector, fromList, length, snoc, unsnoc, (!))
import GeneralLib (PrettyPrintable (prettyPrint))
import MachineInstruction
  ( Arity,
    CodeAddress,
    FOperator (..),
    FunctionName,
    HeapAddress,
    Instruction (..),
    StackAddress,
  )

{- Type definitions -}
data Machine
  = Machine
      Code
      Stack
      InstructionRegister
      ProgramCounter
      ObjectCounter
      Heap
  deriving (Eq, Show)

type Code = V.Vector Instruction

type Stack = V.Vector StackElement

type StackElement = Int

type InstructionRegister = Instruction

type ProgramCounter = Int

type ObjectCounter = Int

type Heap = M.IntMap Object

data Object
  = VAL Integer
  | DEF FunctionName Arity CodeAddress
  | APP HeapAddress HeapAddress
  | IND HeapAddress
  | PRE FOperator
  deriving (Eq, Show, Read)

-- A computation is a monadic action featuring a Machine state and a possible string exception
-- The result is some object of type a
-- type Computation a = ExceptT String (State Machine) a
type Computation m a = ExceptT String (StateT Machine m) a

{--}

{- Define lenses for our machine data type -}
code :: (Functor f) => (Code -> f Code) -> Machine -> f Machine
code f (Machine c s i pc o h) = (\c' -> Machine c' s i pc o h) <$> f c

stack :: (Functor f) => (Stack -> f Stack) -> Machine -> f Machine
stack f (Machine c s i pc o h) = (\s' -> Machine c s' i pc o h) <$> f s

iregister :: (Functor f) => (InstructionRegister -> f InstructionRegister) -> Machine -> f Machine
iregister f (Machine c s i pc o h) = (\i' -> Machine c s i' pc o h) <$> f i

pcounter :: (Functor f) => (ProgramCounter -> f ProgramCounter) -> Machine -> f Machine
pcounter f (Machine c s i pc o h) = (\pc' -> Machine c s i pc' o h) <$> f pc

ocounter :: (Functor f) => (ObjectCounter -> f ObjectCounter) -> Machine -> f Machine
ocounter f (Machine c s i pc o h) = (\o' -> Machine c s i pc o' h) <$> f o

heap :: (Functor f) => (Heap -> f Heap) -> Machine -> f Machine
heap f (Machine c s i pc o h) = (\h' -> Machine c s i pc o h') <$> f h

{--}

{- Utility functions and computations -}
instance PrettyPrintable Machine where
  prettyPrint (Machine _ s ir pc oc h) = "Stack: " ++ show s ++ "\nInstruction register: " ++ show ir ++ "\nProgram counter: " ++ show pc ++ "\nObject counter: " ++ show oc ++ "\nHeap: " ++ show h

getObject :: (Monad m) => HeapAddress -> Computation m Object
getObject a = do
  h <- use heap
  case M.lookup a h of
    Nothing -> throwError $ "getObj: address " ++ show a ++ " out of range!"
    {- In this case we recurse until the last indirection is resolved - this will recurse indefinitely in case there is a loop in the graph! -}
    Just o -> case o of
      (IND h') -> getObject h'
      _ -> return o

push :: (Monad m) => StackElement -> Computation m ()
push n = do
  s <- use stack
  stack .= V.snoc s n

jumpTo :: (Monad m) => CodeAddress -> Computation m ()
jumpTo a = do
  prog <- use code
  if isIndexForVector a prog
    then do
      iregister .= prog V.! a
      pcounter .= a + 1
    else throwError "code address out of range"

loadNextInstruction :: (Monad m) => Computation m ()
loadNextInstruction = do
  pc <- use pcounter
  jumpTo pc

throwError :: (Monad m) => String -> Computation m a
throwError = throwE

createMachine :: [Instruction] -> Maybe Machine
createMachine = createMachineWithHeap []

createMachineWithHeap :: [Object] -> [Instruction] -> Maybe Machine
createMachineWithHeap _ [] = Nothing
createMachineWithHeap os (c : cs) = Just $ Machine (V.fromList (c : cs)) (V.fromList []) c 1 (Prelude.length os) (M.fromList $ I.indexed os)

isIndexForVector :: Int -> V.Vector a -> Bool
isIndexForVector i v = 0 <= i && i < V.length v

pop :: (Monad m) => Computation m StackElement
pop = do
  s <- use stack
  case V.unsnoc s of
    Nothing -> throwError "pop: stack is empty!"
    Just (bottom, top) -> do
      stack .= bottom
      return top

isNotHalted :: (Monad m) => Computation m Bool
isNotHalted = do
  i <- use iregister
  return (i /= Halt)

address :: (Monad m) => FunctionName -> Computation m HeapAddress
address f = do
  h <- use heap
  case lookupFunctionAddress f h of
    Nothing -> throwError $ "address: no such function: " ++ f
    Just a -> return a

lookupFunctionAddress :: FunctionName -> Heap -> Maybe HeapAddress
lookupFunctionAddress f h = case M.lookupMin $ M.filter isFunction h of
  Just (ha, DEF _ _ _) -> Just ha
  _ -> Nothing
  where
    isFunction (DEF f' _ _) = f' == f
    isFunction _ = False

add2arg :: (Monad m) => HeapAddress -> Computation m HeapAddress
add2arg a = do
  o <- getObject a
  case o of
    APP _ a2 -> return a2
    _ -> throwError $ "add2arg: no application found at " ++ show a

new :: (Monad m) => Object -> Computation m HeapAddress
new o = do
  h <- use heap
  ocount <- use ocounter
  heap .= M.insert ocount o h
  ocounter += 1
  return ocount

overrideObject :: (Monad m) => HeapAddress -> Object -> Computation m ()
overrideObject ha newObject = do
  h <- use heap
  heap .= M.adjust (const newObject) ha h

getStackElement :: (Monad m) => StackAddress -> Computation m StackElement
getStackElement sa = do
  s <- use stack
  if isIndexForVector sa s
    then return $ s V.! sa
    else throwError "getStackElement: index out of range!"

stackSize :: (Monad m) => Computation m Int
stackSize = do
  s <- use stack
  return $ V.length s

integerToBool :: Integer -> Bool
integerToBool 0 = False
integerToBool _ = True

boolToInteger :: (Num a) => Bool -> a
boolToInteger False = 0
boolToInteger True = 1

{--}

{- Core computations: step and run
 - This is independent from a concrete computational context like IO to keep things modular
 -}
step :: (Monad m) => Computation m ()
step = do
  currentInstruction <- use iregister
  case currentInstruction of
    Pushfun f -> do
      a <- address f
      push a
      loadNextInstruction
    Pushval n -> do
      a <- new $ VAL n
      push a
      loadNextInstruction
    Pushparam n -> do
      s <- use stack
      let sa = V.length s - n - 2
      ha <- getStackElement sa
      paramAddr <- add2arg ha
      push paramAddr
      loadNextInstruction
    Makeapp -> do
      a1 <- pop
      a2 <- pop
      a <- new $ APP a1 a2
      push a
      loadNextInstruction
    {- stack management and indirection (before evaluating further, since this is called before Unwind + Call, but after expression generation code in function definition) -}
    Update arity -> do
      {- replace subexpression graph in heap by an indirection to the value it evaluates to
        this implements lazy evaluation since this indirection applies to every other expression that points to it -}
      top <- pop
      lenBefore <- stackSize
      let sa = lenBefore - arity - 2
      ha <- getStackElement sa
      overrideObject ha (IND top)

      sndtop <- pop

      {- remove from the stack arity + 1 elements below the topmost two to clear away the unneeded heap addresses from the old expression tree (Update is run after new expression is constructed)  -}
      replicateM_ (arity + 1) pop

      push sndtop
      push top

      loadNextInstruction
    Unwind -> do
      top <- pop
      push top
      obj <- getObject top
      case obj of
        (APP a1 _) -> push a1
        _ -> loadNextInstruction
    Call -> do
      top <- pop
      push top
      obj <- getObject top
      case obj of
        (VAL _) -> loadNextInstruction
        (DEF _ _ a) -> do
          pc <- use pcounter
          push pc
          jumpTo a
        _ -> throwError "Call: Malformed object detected!"
    Return -> do
      res <- pop
      returnAddr <- pop
      push res
      jumpTo returnAddr
    Operator FIf -> do
      -- stack layout on call for if cond then e1 else e2: [e2 (and addr of the if-then-else-application), e1, cond, DEF "if", return addr, cond <- TOP]
      condAddr <- pop
      returnAddr <- pop
      _ <- pop
      _ <- pop
      trueBranchAddr <- pop
      falseBranchAddr <- pop
      -- here we assume that the condObj is already evaluated, since it Operator OpIf is preceded by "Unwind, Call"
      condObj <- getObject condAddr
      resAppAddr <- case condObj of
        (VAL b) -> return (if integerToBool b then trueBranchAddr else falseBranchAddr)
        otherObj -> throwError $ "Operator: non-boolean value in if-condition: " ++ show otherObj
      resAddr <- add2arg resAppAddr

      {- Perform indirection and push elements to stack:
       - first, override the object at falseBranchAddr (the old-if-then-else-expression) by an indirection to the object of the result (at resAddr)
       - then, push the return address followed by the overridden address which now points to the result
       - stack layout after call: [return addr, result <- TOP]
       -}

      overrideObject falseBranchAddr (IND resAddr)

      push returnAddr
      push resAddr

      loadNextInstruction
    Operator op -> do
      -- The order of the operands on the stack for e1 - e2 is [addr(e2), addr(e1), return addr, addr(value(e1)), addr(value(e2))] <- TOP' (structure established by the subroutine for binary operators)
      e2ValAddr <- pop
      e1ValAddr <- pop

      returnAddr <- pop
      _ <- pop
      _ <- pop

      e1ValObj <- getObject e1ValAddr
      e2ValObj <- getObject e2ValAddr
      resObj <- case (e1ValObj, e2ValObj, op) of
        (VAL op1, VAL op2, Smaller) -> return $ VAL $ boolToInteger $ op1 < op2
        (VAL op1, VAL op2, Minus) -> return $ VAL $ op1 - op2
        _ -> throwError "Operator: Type error!"

      {- perform cleanup:
       - the address of e2 is simultaneously the address of the expression that is replaced by the computed result
       - so adjust e2addr to be an indirection to the result's address
       - then push return address and e2addr to the stack
       -}
      e2Addr <- pop
      resAddr <- new resObj
      overrideObject e2Addr (IND resAddr)

      push returnAddr
      push e2Addr

      loadNextInstruction
    Halt -> return ()

run :: (Monad m) => Computation m Object
run = do
  whileM $ do step; isNotHalted
  a <- pop
  getObject a