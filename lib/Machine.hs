{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use record patterns" #-}
module Machine where

import Control.Lens (use, (.=))
import Control.Lens.Operators ((+=))
import Control.Monad.Extra (whileM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, get, runState)
import Data.IntMap as M (IntMap, empty, filter, insert, lookup, lookupMin)
import Data.Vector as V (Vector, fromList, length, snoc, take, unsnoc, (!))
import MachineInstruction
  ( CodeAddress,
    FType (..),
    FunctionName,
    HeapAddress,
    Instruction (..),
    StackCell,
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

type Stack = V.Vector StackCell

type InstructionRegister = Instruction

type ProgramCounter = Int

type ObjectCounter = Int

type Heap = M.IntMap Object

data Object
  = VAL FType Integer
  | DEF FunctionName Int CodeAddress
  | APP HeapAddress HeapAddress
  deriving (Eq, Show)

-- A computation is a monadic action featuring a Machine state and a possible string exception
-- The result might be a string, but it doesn't have to be
type Computation a = ExceptT String (State Machine) a

{--}

{- Define lenses for our machine data type -}
code :: (Functor f) => (Code -> f Code) -> Machine -> f Machine
code f (Machine c s i pc o h) = (\c' -> Machine c' s i pc o h) <$> f c

stack :: (Functor f) => (Stack -> f Stack) -> Machine -> f Machine
stack f (Machine c s i pc o h) = (\s' -> Machine c s' i pc o h) <$> f s

iregister :: (Functor f) => (InstructionRegister -> f InstructionRegister) -> Machine -> f Machine
iregister f (Machine c s i pc o h) = (\i' -> Machine c s i' pc o h) <$> f i

programcounter :: (Functor f) => (ProgramCounter -> f ProgramCounter) -> Machine -> f Machine
programcounter f (Machine c s i pc o h) = (\pc' -> Machine c s i pc' o h) <$> f pc

ocounter :: (Functor f) => (ObjectCounter -> f ObjectCounter) -> Machine -> f Machine
ocounter f (Machine c s i pc o h) = (\o' -> Machine c s i pc o' h) <$> f o

heap :: (Functor f) => (Heap -> f Heap) -> Machine -> f Machine
heap f (Machine c s i pc o h) = (\h' -> Machine c s i pc o h') <$> f h

{--}

{- Common small computations -}

{- Utility functions and computations -}
prettyPrintMachineState :: Machine -> String
prettyPrintMachineState (Machine _ s ir pc oc h) = "Stack: " ++ show s ++ "\nInstruction register: " ++ show ir ++ "\nProgram counter: " ++ show pc ++ "\nObject counter: " ++ show oc ++ "\nHeap: " ++ show h

getObject :: HeapAddress -> Computation Object
getObject a = do
  h <- use heap
  case M.lookup a h of
    Nothing -> throwDiagnosticError $ "heapGetObj: address " ++ show a ++ " out of range!"
    Just o -> return o

push :: StackCell -> Computation ()
push n = do
  s <- use stack
  stack .= V.snoc s n

jumpTo :: CodeAddress -> Computation ()
jumpTo a = do
  prog <- use code
  if isIndexForVector a prog
    then do
      iregister .= prog V.! a
      programcounter .= a + 1
    else throwDiagnosticError "code address out of range"

throwDiagnosticError :: String -> Computation a
throwDiagnosticError e = do
  m <- lift get
  throwE $ e ++ "\n" ++ prettyPrintMachineState m

createMachine :: [Instruction] -> Maybe Machine
createMachine [] = Nothing
createMachine (c : cs) = Just $ Machine (V.fromList (c : cs)) (V.fromList []) c 1 0 M.empty

isIndexForVector :: Int -> V.Vector a -> Bool
isIndexForVector i v = 0 <= i && i < V.length v

pop :: Computation StackCell
pop = do
  s <- use stack
  case V.unsnoc s of
    Nothing -> throwDiagnosticError "pop: stack is empty!"
    Just (bottom, top) -> do
      stack .= bottom
      return top

isNotHalted :: Computation Bool
isNotHalted = do
  i <- use iregister
  return (i /= Halt)

address :: FunctionName -> Computation HeapAddress
address f = do
  h <- use heap
  case lookupFunctionAddress f h of
    Nothing -> throwDiagnosticError $ "address: no such function: " ++ f
    Just a -> return a

lookupFunctionAddress :: FunctionName -> Heap -> Maybe HeapAddress
lookupFunctionAddress f h = case M.lookupMin $ M.filter isFunction h of
  Just (ha, DEF _ _ _) -> Just ha
  _ -> Nothing
  where
    isFunction (DEF f' _ _) = f' == f
    isFunction _ = False

add2arg :: HeapAddress -> Computation HeapAddress
add2arg a = do
  o <- getObject a
  case o of
    APP _ a2 -> return a2
    _ -> throwDiagnosticError $ "add2arg: no application found at " ++ show a

new :: Object -> Computation HeapAddress
new o = do
  h <- use heap
  ocount <- use ocounter
  heap .= M.insert ocount o h
  ocounter += 1
  return ocount

objType :: HeapAddress -> Computation FType
objType a = do
  o <- getObject a
  case o of
    VAL t _ -> return t
    _ -> throwDiagnosticError $ "objType: object " ++ show o ++ " isn't of VAL type!"

{--}

{- Core computations: step and run
 - This is independent from a concrete computational context like IO to keep things modular
 -}
step :: Computation ()
step = do
  currentInstruction <- use iregister
  case currentInstruction of
    {- Add function definition to heap
       This is a replacement for the otherwise necessary predefined global function definition environment -}
    AddDef f n a -> do
      _ <- new $ DEF f n a
      pc <- use programcounter
      jumpTo pc
    Pushfun f -> do
      a <- address f
      push $ toInteger a
      pc <- use programcounter
      jumpTo pc
    Pushval t n -> do
      a <- new $ VAL t n
      push $ toInteger a
      pc <- use programcounter
      jumpTo pc
    Pushparam n -> do
      s <- use stack
      let sa = V.length s - n - 2
      if isIndexForVector sa s
        then do
          let ha = fromInteger $ s V.! sa
          paramAddr <- add2arg ha
          push $ toInteger paramAddr
        else throwDiagnosticError "Pushparam: index out of range!"
      pc <- use programcounter
      jumpTo pc
    Makeapp -> do
      a1 <- pop
      a2 <- pop
      a <- new $ APP (fromInteger a1) (fromInteger a2)
      push $ toInteger a
      pc <- use programcounter
      jumpTo pc
    Slide n -> do
      top <- pop
      sndtop <- pop
      s <- use stack
      let newLength = V.length s - n
      if newLength >= 0
        then do
          stack .= V.take newLength s
          push sndtop
          push top
        else throwDiagnosticError "Slide: can't slide so many elements, stack too small!"
      pc <- use programcounter
      jumpTo pc
    Reduce -> do
      top <- pop
      o <- getObject $ fromInteger top
      case o of
        (APP a1 _) -> do
          push top
          push $ toInteger a1
        (DEF _ _ a) -> do
          push top
          pc <- use programcounter
          {- Here we don't need to push pc + 1 because pc already points to the next instruction as per the instruction cycle -}
          push $ toInteger pc
          jumpTo a
        (VAL _ _) -> do
          returnAddr <- pop
          push top
          jumpTo $ fromInteger returnAddr
    {- TODO -}
    Unwind -> do throwDiagnosticError "Unwind: Not implemented!"
    {- TODO -}
    Call -> do throwDiagnosticError "Call: Not implemented!"
    Return -> do
      res <- pop
      returnAddr <- pop
      push res
      jumpTo $ fromInteger returnAddr
    {- TODO -}
    Pushpre _ -> do throwDiagnosticError "Pushpre: Not implemented!"
    {- TODO -}
    Update _ -> do throwDiagnosticError "Update: Not implemented!"
    {- TODO -}
    Operator _ -> do throwDiagnosticError "Operator: Not implemented!"
    Halt -> do return ()

run :: Computation Object
run = do
  whileM $ do step; isNotHalted
  a <- pop
  getObject $ fromInteger a

runProgram :: [Instruction] -> String
runProgram prog = case createMachine prog of
  Nothing -> "Invalid machine code"
  Just m -> case runState (runExceptT run) m of
    (Left msg, _) -> msg
    (Right (VAL t v), _) -> case t of
      FInteger -> show v
      FBool -> if v == 0 then "False" else "True"
    (Right o, _) -> "Return value is malformed (" ++ show o ++ ")"