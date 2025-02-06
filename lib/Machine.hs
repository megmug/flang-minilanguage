{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use record patterns" #-}
module Machine where

import Control.Lens (use, (.=))
import Control.Lens.Operators ((+=))
import Control.Monad.Extra (whileM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State (StateT, get)
import Data.IntMap as M (IntMap, adjust, filter, fromList, insert, lookup, lookupMin)
import Data.List.Index as I (indexed)
import Data.Vector as V (Vector, fromList, length, snoc, take, unsnoc, (!))
import MachineInstruction
  ( Arity,
    CodeAddress,
    FOperator (..),
    FType (..),
    FunctionName,
    HeapAddress,
    Instruction (..),
    OperatorArg (..),
    StackAddress,
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

type StackElement = Int

type InstructionRegister = Instruction

type ProgramCounter = Int

type ObjectCounter = Int

type Heap = M.IntMap Object

data Object
  = VAL FType Integer
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
prettyPrintMachineState :: Machine -> String
prettyPrintMachineState (Machine _ s ir pc oc h) = "Stack: " ++ show s ++ "\nInstruction register: " ++ show ir ++ "\nProgram counter: " ++ show pc ++ "\nObject counter: " ++ show oc ++ "\nHeap: " ++ show h

getObject :: (Monad m) => HeapAddress -> Computation m Object
getObject a = do
  h <- use heap
  case M.lookup a h of
    Nothing -> throwError $ "heapGetObj: address " ++ show a ++ " out of range!"
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
throwError e = do
  m <- lift get
  throwE $ e ++ "\n" ++ prettyPrintMachineState m

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

objType :: (Monad m) => HeapAddress -> Computation m FType
objType a = do
  o <- getObject a
  case o of
    VAL t _ -> return t
    _ -> throwError $ "objType: object " ++ show o ++ " isn't of VAL type!"

getStackElement :: (Monad m) => StackAddress -> Computation m StackElement
getStackElement sa = do
  s <- use stack
  if isIndexForVector sa s
    then return $ s V.! sa
    else throwError "getStackElement: index out of range!"

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
    {- Add function definition to heap
       This is a replacement for the otherwise necessary predefined global function definition environment -}
    AddDef f n a -> do
      _ <- new $ DEF f n a
      loadNextInstruction
    {- NOOP for compatibility sake -}
    Reset -> loadNextInstruction
    Pushfun f -> do
      a <- address f
      push a
      loadNextInstruction
    Pushval t n -> do
      a <- new $ VAL t n
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
        else throwError "Slide: can't slide so many elements, stack too small!"
      loadNextInstruction
    Reduce -> do
      top <- pop
      o <- getObject top
      case o of
        (APP a1 _) -> do
          push top
          push a1
        (DEF _ _ a) -> do
          push top
          pc <- use pcounter
          {- Here we don't need to push pc + 1 because pc already points to the next instruction as per the instruction cycle -}
          push pc
          jumpTo a
        (VAL _ _) -> do
          returnAddr <- pop
          push top
          jumpTo returnAddr
        _ -> throwError "Reduce: Malformed object detected!"
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
        (VAL _ _) -> loadNextInstruction
        (DEF _ _ a) -> do
          pc <- use pcounter
          push pc
          jumpTo a
        {- binary operator-}
        (PRE op)
          | op `elem` [Equals, Smaller, Plus, Minus, Times, Divide, And, Or] -> do
              pc <- use pcounter
              push pc
              {- push representation of operator onto stack -}
              push top
              {- this is nasty - but according to spec! -}
              jumpTo 4
          {- ternary operator: if-}
          | op == FIf -> do
              pc <- use pcounter
              push pc
              {- push representation of operator onto stack 
               - this is different in the spec, but otherwise, the generated PushParam offset is wrong, so this seems to be the intended behaviour
               -}
              push top
              {- this is nasty - but according to spec! -}
              jumpTo 13
          {- unary operator: otherwise op == Not -}
          | otherwise -> do
              pc <- use pcounter
              push pc
              {- push representation of operator onto stack -}
              push top
              jumpTo 19
        _ -> throwError "Call: Malformed object detected!"
    Return -> do
      res <- pop
      returnAddr <- pop
      push res
      jumpTo returnAddr
    Pushpre op -> do
      obj <- new $ PRE op
      push obj
      loadNextInstruction
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
          overrideObject oldExprAddr (IND newValAddr)
        Arity n -> do
          {- replace subexpression graph in heap by an indirection to the value it evaluates to
             this implements lazy evaluation since this indirection applies to every other expression that points to it -}
          s <- use stack
          top <- pop
          push top
          let sa = V.length s - n - 3
          ha <- getStackElement sa
          overrideObject ha (IND top)
      loadNextInstruction
    Operator op -> case op of
      One -> do
        operandAddr <- pop
        opAddr <- pop
        returnAddr <- pop
        _ <- pop
        op' <- getObject opAddr
        if op' /= PRE Not
          then throwError "Operator: Type error!"
          else do
            push returnAddr
            {- Logically negate the operand, create resulting VAL on the heap and push result address onto stack -}
            operandObj <- getObject operandAddr
            case operandObj of
              (VAL FBool b) -> do
                res <- new $ VAL FBool (boolToInteger $ not $ integerToBool b)
                push res
              _ -> throwError "Operator: Type error!"
        loadNextInstruction
      Two -> do
        -- The order of the operands on the stack is 'operator, operand 1, operand 2 <- TOP' (structure established by the subroutine for binary operators)
        operand2Addr <- pop
        operand1Addr <- pop

        opAddr <- pop
        returnAddr <- pop
        _ <- pop
        _ <- pop
        opObj <- getObject opAddr
        operand1Obj <- getObject operand1Addr
        operand2Obj <- getObject operand2Addr
        resObj <- case (operand1Obj, operand2Obj, opObj) of
          (VAL FInteger op1, VAL FInteger op2, PRE Equals) -> return $ VAL FBool $ boolToInteger $ op1 == op2
          (VAL FBool op1, VAL FBool op2, PRE Equals) -> return $ VAL FBool $ boolToInteger $ op1 == op2
          (VAL FInteger op1, VAL FInteger op2, PRE Smaller) -> return $ VAL FBool $ boolToInteger $ op1 < op2
          (VAL FInteger op1, VAL FInteger op2, PRE Plus) -> return $ VAL FInteger $ op1 + op2
          (VAL FInteger op1, VAL FInteger op2, PRE Minus) -> return $ VAL FInteger $ op1 - op2
          (VAL FInteger op1, VAL FInteger op2, PRE Times) -> return $ VAL FInteger $ op1 * op2
          (VAL FInteger op1, VAL FInteger op2, PRE Divide) -> return $ VAL FInteger $ op1 `div` op2
          -- implementing and as multiplication
          (VAL FBool op1, VAL FBool op2, PRE And) -> return $ VAL FBool $ op1 * op2
          -- for Or, addition doesn't quite work, e.g. -1 + 1 = 0
          (VAL FBool op1, VAL FBool op2, PRE Or) -> return $ VAL FBool $ boolToInteger $ integerToBool op1 || integerToBool op2
          _ -> throwError "Operator: Type error!"

        push returnAddr
        resAddr <- new resObj
        push resAddr

        loadNextInstruction
      OpIf -> do
        condAddr <- pop
        _ <- pop
        returnAddr <- pop
        _ <- pop
        _ <- pop
        trueBranchAddr <- pop
        falseBranchAddr <- pop
        -- here we assume that the condObj is already evaluated, since it Operator OpIf is preceded by "Unwind, Call"
        condObj <- getObject condAddr
        resAppAddr <- case condObj of
          (VAL FBool b) -> return (if integerToBool b then trueBranchAddr else falseBranchAddr)
          otherObj -> throwError $ "Operator: non-boolean value in if-condition: " ++ show otherObj
        resAddr <- add2arg resAppAddr

        {- Push elements to stack - the spec is wrong, but it makes sense to do this:
         - push the address of the false branch first - this is the address of the if expression, since it is an application of (if cond then trueBranch else) to falseBranch
         - then push the return address
         - then push the address of the resulting expression
         - this must be the order since "Operator OpIf" is followed by "Update PredefinedOperator" which expects  the order "expression to indirect, return address, target of indirection <- TOP"
         -}
        push falseBranchAddr
        push returnAddr
        push resAddr

        loadNextInstruction
    Halt -> do return ()

run :: (Monad m) => Computation m Object
run = do
  whileM $ do step; isNotHalted
  a <- pop
  getObject a