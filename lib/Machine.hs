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
import Data.IntMap as M (IntMap, adjust, filter, fromList, insert, lookup, lookupMin)
import Data.List.Index as I (indexed)
import Data.Vector as V (Vector, fromList, length, snoc, take, unsnoc, (!))
import MachineInstruction
  ( CodeAddress,
    FOperator (..),
    FType (..),
    FunctionName,
    HeapAddress,
    Instruction (..),
    OperatorArg (..),
    StackAddress,
    StackElement,
    UpdateArg (..),
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

type InstructionRegister = Instruction

type ProgramCounter = Int

type ObjectCounter = Int

type Heap = M.IntMap Object

data Object
  = VAL FType Integer
  | DEF FunctionName Int CodeAddress
  | APP HeapAddress HeapAddress
  | IND HeapAddress
  | PRE FOperator
  deriving (Eq, Show, Read)

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

{- Utility functions and computations -}
prettyPrintMachineState :: Machine -> String
prettyPrintMachineState (Machine _ s ir pc oc h) = "Stack: " ++ show s ++ "\nInstruction register: " ++ show ir ++ "\nProgram counter: " ++ show pc ++ "\nObject counter: " ++ show oc ++ "\nHeap: " ++ show h

getObject :: HeapAddress -> Computation Object
getObject a = do
  h <- use heap
  case M.lookup a h of
    Nothing -> throwDiagnosticError $ "heapGetObj: address " ++ show a ++ " out of range!"
    {- In this case we recurse until the last indirection is resolved - this will recurse indefinitely in case there is a loop in the graph! -}
    Just o -> case o of
      (IND h') -> getObject h'
      _ -> return o

push :: StackElement -> Computation ()
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
createMachine = createMachineWithHeap []

createMachineWithHeap :: [Object] -> [Instruction] -> Maybe Machine
createMachineWithHeap _ [] = Nothing
createMachineWithHeap os (c : cs) = Just $ Machine (V.fromList (c : cs)) (V.fromList []) c 1 (Prelude.length os) (M.fromList $ I.indexed os)

isIndexForVector :: Int -> V.Vector a -> Bool
isIndexForVector i v = 0 <= i && i < V.length v

pop :: Computation StackElement
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

overrideObject :: HeapAddress -> Object -> Computation ()
overrideObject ha newObject = do
  h <- use heap
  heap .= M.adjust (const newObject) ha h

objType :: HeapAddress -> Computation FType
objType a = do
  o <- getObject a
  case o of
    VAL t _ -> return t
    _ -> throwDiagnosticError $ "objType: object " ++ show o ++ " isn't of VAL type!"

getStackElement :: StackAddress -> Computation StackElement
getStackElement sa = do
  s <- use stack
  if isIndexForVector sa s
    then do
      return $ fromInteger $ s V.! sa
    else throwDiagnosticError "getStackElement: index out of range!"

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
    {- NOOP for compatibility sake -}
    Reset -> do
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
      ha <- getStackElement sa
      paramAddr <- add2arg $ fromInteger ha
      push $ toInteger paramAddr
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
        _ -> throwDiagnosticError "Reduce: Malformed object detected!"
    Unwind -> do
      top <- pop
      push top
      obj <- getObject $ fromInteger top
      case obj of
        (APP a1 _) -> push $ toInteger a1
        _ -> do
          pc <- use programcounter
          jumpTo pc
    Call -> do
      top <- pop
      push top
      obj <- getObject $ fromInteger top
      case obj of
        (VAL _ _) -> do
          pc <- use programcounter
          jumpTo pc
        (DEF _ _ a) -> do
          pc <- use programcounter
          push $ toInteger pc
          jumpTo a
        {- binary operator-}
        (PRE op)
          | op `elem` [Equals, Smaller, Plus, Minus, Times, Divide, And, Or] -> do
              pc <- use programcounter
              push $ toInteger pc
              {- push representation of operator onto stack -}
              push top
              {- this is nasty - but according to spec! -}
              jumpTo 4
          {- ternary operator: if-}
          | op == FIf -> do
              pc <- use programcounter
              push $ toInteger pc
              {- this is nasty - but according to spec! -}
              jumpTo 13
          {- unary operator: otherwise op == Not -}
          | otherwise -> do
              pc <- use programcounter
              push $ toInteger pc
              {- push representation of operator onto stack -}
              push top
              jumpTo 19
        _ -> throwDiagnosticError "Call: Malformed object detected!"
    Return -> do
      res <- pop
      returnAddr <- pop
      push res
      jumpTo $ fromInteger returnAddr
    Pushpre op -> do
      obj <- new $ PRE op
      push $ toInteger obj
      pc <- use programcounter
      jumpTo pc
    Update arg -> do
      case arg of
        PredefinedOperator -> do
          {- in case of a predefined operator, discard top element and swap remaining two top elements -}
          newValAddr <- pop
          returnAddr <- pop
          oldExprAddr <- pop
          push returnAddr
          push oldExprAddr
          {- Adjust heap cell at old expression address 
             Weirdly, this is missing in the spec but necessary to implement the (lazy) evaluation correctly 
             It is mentioned briefly in the introduction of the new instructions for complete MF though -}
          overrideObject (fromInteger oldExprAddr) (IND $ fromInteger newValAddr)
        Arity n -> do
          {- replace subexpression graph in heap by an indirection to the value it evaluates to
             this implements lazy evaluation since this indirection applies to every other expression that points to it -}
          s <- use stack
          top <- pop
          push top
          let sa = V.length s - n - 3
          ha <- getStackElement sa
          overrideObject (fromInteger ha) (IND $ fromInteger top)
      pc <- use programcounter
      jumpTo pc
    {- TODO -}
    Operator op -> case op of
      One -> do
        operandAddr <- pop
        opAddr <- pop
        returnAddr <- pop
        _ <- pop
        op' <- getObject $ fromInteger opAddr
        if op' /= PRE Not
          then throwDiagnosticError "Operator: Type error!"
          else do
            push returnAddr
            {- Logically negate the operand, create resulting VAL on the heap and push result address onto stack -}
            operandObj <- getObject $ fromInteger operandAddr
            case operandObj of
              (VAL FBool b) -> do
                res <- new $ VAL FBool (boolToInteger $ not $ integerToBool b)
                push $ toInteger res
              _ -> throwDiagnosticError "Operator: Type error!"
        pc <- use programcounter
        jumpTo pc
      Two -> do
        operand1Addr <- pop
        operand2Addr <- pop
        opAddr <- pop
        returnAddr <- pop
        _ <- pop
        _ <- pop
        opObj <- getObject $ fromInteger opAddr
        operand1Obj <- getObject $ fromInteger operand1Addr
        operand2Obj <- getObject $ fromInteger operand2Addr
        resObj <- case (operand1Obj, operand2Obj, opObj) of
          (VAL FInteger op1, VAL FInteger op2, PRE Equals) -> return $ VAL FInteger $ boolToInteger $ op1 == op2
          (VAL FBool op1, VAL FBool op2, PRE Equals) -> return $ VAL FBool $ boolToInteger $ op1 == op2
          (VAL FInteger op1, VAL FInteger op2, PRE Smaller) -> return $ VAL FInteger $ boolToInteger $ op1 < op2
          (VAL FInteger op1, VAL FInteger op2, PRE Plus) -> return $ VAL FInteger $ op1 + op2
          (VAL FInteger op1, VAL FInteger op2, PRE Minus) -> return $ VAL FInteger $ op1 - op2
          (VAL FInteger op1, VAL FInteger op2, PRE Times) -> return $ VAL FInteger $ op1 * op2
          (VAL FInteger op1, VAL FInteger op2, PRE Divide) -> return $ VAL FInteger $ op1 `div` op2
          -- implementing and as multiplication
          (VAL FBool op1, VAL FBool op2, PRE And) -> return $ VAL FBool $ op1 * op2
          -- for Or, addition doesn't quite work, e.g. -1 + 1 = 0
          (VAL FBool op1, VAL FBool op2, PRE Or) -> return $ VAL FBool $ boolToInteger $ integerToBool op1 || integerToBool op2
          _ -> throwDiagnosticError "Operator: Type error!"

        push returnAddr
        resAddr <- new resObj
        push $ toInteger resAddr

        pc <- use programcounter
        jumpTo pc
      OpIf -> do
        condAddr <- pop
        _ <- pop
        returnAddr <- pop
        _ <- pop
        trueBranchAddr <- pop
        falseBranchAddr <- pop

        condObj <- getObject $ fromInteger condAddr
        resAppAddr <- case condObj of
          (VAL FBool b) -> return (if integerToBool b then trueBranchAddr else falseBranchAddr)
          _ -> throwDiagnosticError "Operator: Type error"
        resAddr <- add2arg $ fromInteger resAppAddr

        {- Push elements to stack - the spec is wrong, so unsure what exactly is the right thing to do here (?) -}
        push returnAddr
        push $ toInteger resAddr

        pc <- use programcounter
        jumpTo pc
    Halt -> do return ()

run :: Computation Object
run = do
  whileM $ do step; isNotHalted
  a <- pop
  getObject $ fromInteger a

runProgram :: [Object] -> [Instruction] -> String
runProgram h prog = case createMachineWithHeap h prog of
  Nothing -> "Invalid machine code"
  Just m -> case runState (runExceptT run) m of
    (Left msg, _) -> msg
    (Right (VAL t v), _) -> case t of
      FInteger -> show v
      FBool -> if v == 0 then "False" else "True"
    (Right o, _) -> "Return value is malformed (" ++ show o ++ ")"