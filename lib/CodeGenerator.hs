{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use record patterns" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CodeGenerator where

import Control.Lens (use)
import Control.Lens.Operators ((%=), (.=))
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState, get)
import Data.Foldable (traverse_)
import Data.List (nub)
import Data.List.Index (indexed)
import Machine (Object (DEF))
import MachineInstruction
  ( Arity,
    FOperator
      ( Divide,
        FIf,
        Minus,
        Smaller,
        Times
      ),
    Instruction
      ( Call,
        Halt,
        Makeapp,
        Operator,
        Pushfun,
        Pushparam,
        Pushpre,
        Pushval,
        Return,
        Slide,
        Unwind,
        Update
      ),
    OperatorArg (OpIf, Two),
    UpdateArg (Arity, PredefinedOperator),
  )
import SyntaxTree
  ( Definition (..),
    Expression (..),
    Program (..),
    Stage (Core),
    VariableName,
    prettyPrint,
  )

{- The code generator maintains a state that carries:
    - the current prefix length (how many machine instructions precede the one that will be generated next?)
    - a position list that specifies a mapping of names to their respective positions in the parameter list
    - the code that was already generated, maintained as a simple list
    - a heap environment that is used to store the generated DEF function definitions that the machine requires for operation
 -}
data GenState = GenState PosList Code HeapEnvironment GlobalFunctionList

type PrefixLength = Int

type PosList = [(Int, VariableName)]

type Code = [Instruction]

type HeapEnvironment = [Object]

type GlobalFunctionList = [(VariableName, Arity)]

-- Here we define a type for monadic actions that represent the types of our code generators
type Generator a = ExceptT String (State GenState) a

{- Lens definitions for GenState -}
posList :: (Functor f) => (PosList -> f PosList) -> GenState -> f GenState
posList f (GenState pos c h funcs) = (\pos' -> GenState pos' c h funcs) <$> f pos

code :: (Functor f) => (Code -> f Code) -> GenState -> f GenState
code f (GenState pos c h funcs) = (\code' -> GenState pos code' h funcs) <$> f c

heapEnv :: (Functor f) => (HeapEnvironment -> f HeapEnvironment) -> GenState -> f GenState
heapEnv f (GenState pos c h funcs) = (\h' -> GenState pos c h' funcs) <$> f h

globalFuncs :: (Functor f) => (GlobalFunctionList -> f GlobalFunctionList) -> GenState -> f GenState
globalFuncs f (GenState pos c h funcs) = (\funcs' -> GenState pos c h funcs') <$> f funcs

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
        (GenState _ c h _) <- lift get
        return (h, c)

  -- This runs a generator with some default empty state (mostly useful for whole programs)
  generate :: a -> Either String (HeapEnvironment, Code)
  generate e = customGenerate e $ GenState [] [] [] []

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
           -- subroutine for binary operators (address: 3)
           Pushparam 2,
           Unwind,
           Call,
           Pushparam 4,
           Unwind,
           Call,
           Operator Two,
           Update PredefinedOperator,
           Return,
           -- subroutine for if-then-else operator (address: 12)
           Pushparam 2,
           Unwind,
           Call,
           Operator OpIf,
           Update PredefinedOperator,
           -- These instructions are to evaluate the resulting expression as well, since this is the intended behaviour for if-then-else expressions
           Unwind, --
           Call,
           Return
         ]
    -- to understand why we need to generate function defs iteratively, see generateDefs
    traverse_ addToGlobalFuncs defs
    -- check if main definition is missing - if so, abort
    funs <- use globalFuncs
    unless ("main" `elem` map fst funs) $ throwError "no main definition!"
    -- generate all definitions
    traverse_ generator defs

instance Generatable (Definition Core) where
  generator def@(Definition f params e) = do
    when (params /= nub params) $ throwError $ "function definition " ++ prettyPrint def ++ " has conflicting parameter bindings!"
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
    code %= (++ [Update (Arity n), Slide (n + 1), Unwind, Call, Return])
    -- reset posList (compile-time cleanup)
    posList .= []

instance Generatable (Expression Core) where
  generator (IfThenElse e1 e2 e3) = do
    generator e3
    posList %= posPlus 1
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre FIf, Makeapp, Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-2)
  generator (SyntaxTree.Smaller e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre MachineInstruction.Smaller, Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-1)
  generator (Difference e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre MachineInstruction.Minus, Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-1)
  generator (Quotient e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre Divide, Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-1)
  generator (Product e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre Times, Makeapp, Makeapp])
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
      -- if it doesn't, we assume it is a function (of course this is unsafe, but this is better implemented as part of a proper type checker that doesn't yet exist)
      Nothing -> do
        funs <- use globalFuncs
        unless (v `isDefinedIn` funs) $ throwError $ "no such function: " ++ v
        code %= (++ [Pushfun v])
  generator (Number n) = do
    code %= (++ [Pushval n])

{--}

{- Helper functions and generators -}

{- -}
addToGlobalFuncs :: Definition Core -> Generator ()
addToGlobalFuncs def@(Definition f params _) = do
  -- add new function to global environment, if it is not already defined - otherwise, throw error
  funs <- use globalFuncs
  when (f `isDefinedIn` funs) $ throwError $ "conflicting function definition for function " ++ f ++ ": " ++ prettyPrint def
  globalFuncs %= (++ [(f, length params)])

throwError :: String -> Generator ()
throwError s = throwE $ "Error during code generation: " ++ s

paramsToPosList :: [a] -> [(Int, a)]
paramsToPosList = posPlus 1 . indexed

posPlus :: Int -> [(Int, a)] -> [(Int, a)]
posPlus i = map (\(ind, a) -> (ind + i, a))

lookupPos :: (Eq a) => a -> [(Int, a)] -> Maybe Int
lookupPos _ [] = Nothing
lookupPos y ((i, x) : xs) = if x == y then Just i else lookupPos y xs

isDefinedIn :: VariableName -> GlobalFunctionList -> Bool
isDefinedIn f fs = f `elem` funs
  where
    funs = map fst fs

{--}