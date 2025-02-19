{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use record patterns" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CodeGenerator where

import Control.Lens (use)
import Control.Lens.Operators ((%=), (.=))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState, get)
import Data.Foldable (traverse_)
import Data.List.Index (indexed)
import Machine (Object (DEF))
import MachineInstruction
  ( FOperator
      ( If,
        Minus,
        Smaller
      ),
    Instruction
      ( Call,
        Halt,
        Makeapp,
        Operator,
        Pushfun,
        Pushparam,
        Pushval,
        Return,
        Unwind,
        Update
      ),
  )
import SyntaxTree
  ( Definition (..),
    Expression (..),
    Program (..),
    Stage (Core),
    VariableName,
  )

{- The code generator maintains a state that carries:
    - the current prefix length (how many machine instructions precede the one that will be generated next?)
    - a position list that specifies a mapping of names to their respective positions in the parameter list
    - the code that was already generated, maintained as a simple list
    - a heap environment that is used to store the generated DEF function definitions that the machine requires for operation
 -}
data GenState = GenState PosList Code HeapEnvironment

type PrefixLength = Int

type PosList = [(Int, VariableName)]

type Code = [Instruction]

type HeapEnvironment = [Object]

-- Here we define a type for monadic actions that represent the types of our code generators
type Generator a = ExceptT String (State GenState) a

{- Lens definitions for GenState -}
posList :: (Functor f) => (PosList -> f PosList) -> GenState -> f GenState
posList f (GenState pos c h) = (\pos' -> GenState pos' c h) <$> f pos

code :: (Functor f) => (Code -> f Code) -> GenState -> f GenState
code f (GenState pos c h) = (\code' -> GenState pos code' h) <$> f c

heapEnv :: (Functor f) => (HeapEnvironment -> f HeapEnvironment) -> GenState -> f GenState
heapEnv f (GenState pos c h) = (\h' -> GenState pos c h') <$> f h

{- -}

{- Type class for syntactical elements that can be compiled -}
class Generatable a where
  {- A generator creates a monadic action from a syntactical element that can generate code for it
   -}
  generator :: a -> Generator ()

  -- This runs a generator with some supplied state (can also be useful for testing)
  customGenerate :: a -> GenState -> Either String (HeapEnvironment, Code)
  customGenerate e = evalState (runExceptT generateAndReturn)
    where
      generateAndReturn = do
        generator e
        (GenState _ c h) <- lift get
        return (h, c)

  -- This runs a generator with some default empty state (mostly useful for whole programs)
  generate :: a -> Either String (HeapEnvironment, Code)
  generate e = customGenerate e $ GenState [] [] []

{--}

{- Class instances -}
{- Convention: Every generator ensures to clean up its state after itself -}
instance Generatable (Program Core) where
  generator (Program defs) = do
    code
      -- main program entry point
      .= [ Pushfun "main",
           Call,
           Halt,
           -- subroutine for smaller operator (address: 3)
           Pushparam 1,
           Unwind,
           Call,
           Pushparam 3,
           Unwind,
           Call,
           Operator MachineInstruction.Smaller,
           Return,
           -- subroutine for minus operator (address: 11)
           Pushparam 1,
           Unwind,
           Call,
           Pushparam 3,
           Unwind,
           Call,
           Operator MachineInstruction.Minus,
           Return,
           -- subroutine for if-then-else operator (address: 19)
           Pushparam 1,
           Unwind,
           Call,
           Operator If,
           -- These instructions are to evaluate the resulting expression as well, since this is the intended behaviour for if-then-else expressions
           Unwind,
           Call,
           Return
         ]
    -- add function definitions for predefined functions
    heapEnv .= [DEF "<" 2 3, DEF "-" 2 11, DEF "#if" 3 19]
    -- generate all definitions
    traverse_ generator defs

instance Generatable (Definition Core) where
  generator (Definition f params e) = do
    c <- use code
    let addr = length c
    let n = length params
    -- add new function DEF to heap environment (it is already added to the global functions at the program-generator level)
    heapEnv %= (++ [DEF f n addr])
    -- add params to posList
    posList .= paramsToPosList params
    -- generate defining expression
    generator e
    -- append suffix for cleanup (at runtime) and returning
    code %= (++ [Update n, Unwind, Call, Return])
    -- reset posList (compile-time cleanup)
    posList .= []

instance Generatable (Expression Core) where
  generator (IfThenElse e1 e2 e3) = do
    generator e3
    posList %= posPlus 1
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushfun "#if", Makeapp, Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-2)
  generator (SyntaxTree.Smaller e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushfun "<", Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-1)
  generator (Difference e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushfun "-", Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-1)
  generator (Application e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Makeapp])
    {- reset the posList again
       this is necessary because in the script, the posList management is defined recursively, but we carry the state until after the recursive invocation -}
    posList %= posPlus (-1)
  generator (Variable v) = do
    positions <- use posList
    case lookupPos v positions of
      -- if it has a position it is a parameter
      Just i -> code %= (++ [Pushparam i])
      -- if it doesn't, we assume it is a function (this is safe since the typifier rules out the possibility of undefined functions)
      Nothing -> code %= (++ [Pushfun v])
  generator (Number n) = do
    code %= (++ [Pushval n])

{--}

{- Helper functions and generators -}

{- -}

throwError :: String -> Generator ()
throwError s = throwE $ "Error during code generation: " ++ s

paramsToPosList :: [a] -> [(Int, a)]
paramsToPosList = posPlus 1 . indexed

posPlus :: Int -> [(Int, a)] -> [(Int, a)]
posPlus i = map (\(ind, a) -> (ind + i, a))

lookupPos :: (Eq a) => a -> [(Int, a)] -> Maybe Int
lookupPos _ [] = Nothing
lookupPos y ((i, x) : xs) = if x == y then Just i else lookupPos y xs

{--}