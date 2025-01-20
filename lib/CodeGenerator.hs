{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use record patterns" #-}
module CodeGenerator where

import Control.Lens (use)
import Control.Lens.Operators ((%=), (.=))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState, get)
import Data.Foldable (traverse_)
import Data.List.Index (indexed)
import Machine (Object (DEF), boolToInteger)
import MachineInstruction
import SyntaxTree

{- The code generator maintains a state that carries:
    - the current prefix length (how many machine instructions precede the one that will be generated next?)
    - a position list that specifies a mapping of names to their respective positions in the parameter list
    - the code that was already generated, maintained as a simple list
    - a heap environment that is used to store the generated DEF function definitions that the machine requires for operation
 -}
data GenState = GenState PosList Code HeapEnvironment

type PrefixLength = Int

type PosList = [(Int, String)]

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
instance Generatable Program where
  generator (Program defs) = do
    code
      .= [ Reset,
           Pushfun "main",
           Reduce,
           Halt
         ]
    traverse_ generator defs

instance Generatable Definition where
  generator (Definition f params e) = do
    c <- use code
    let addr = length c
    let n = length params
    -- add new function DEF to heap environment
    heapEnv %= (++ [DEF f n addr])
    -- add params to posList
    posList .= paramsToPosList params
    -- generate defining expression
    generator e
    -- append suffix for cleanup (at runtime) and returning
    code %= (++ [Slide (n + 1), Reduce, Return])
    -- reset posList (compile-time cleanup)
    posList .= []

instance Generatable LocalDefinition where
  generator (LocalDefinition _ _) = throwError "local definitions unsupported" -- TODO

instance Generatable Expression where
  generator (Let _ _) = throwError "let-expressions unsupported" -- TODO
  generator (IfThenElse _ _ _) = throwError "if-then-else unsupported" -- TODO
  generator (Disjunction _ _) = throwError "logical disjunction unsupported" -- TODO
  generator (Conjunction _ _) = throwError "logical conjunction unsupported" -- TODO
  generator (LogicalNegation _) = throwError "logical negation unsupported" -- TODO
  generator (SyntaxTree.Smaller _ _) = throwError "lt-expressions unsupported" -- TODO
  generator (Equality _ _) = throwError "equality expressions unsupported" -- TODO
  generator (SyntaxTree.Minus _) = throwError "minus expressions unsupported" -- TODO
  generator (Difference _ _) = throwError "difference expressions unsupported" -- TODO
  generator (Sum _ _) = throwError "sum expressions unsupported" -- TODO
  generator (Quotient _ _) = throwError "quotient expressions unsupported" -- TODO
  generator (Product _ _) = throwError "product expressions unsupported" -- TODO
  generator (Application e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Makeapp])
  generator (Variable v) = do
    positions <- use posList
    case lookupPos v positions of
      -- if it has a position it is a parameter
      Just i -> code %= (++ [Pushparam i])
      -- if it doesn't, we assume it is a function (of course this is unsafe, but this is better implemented as part of a proper type checker that doesn't yet exist)
      Nothing -> code %= (++ [Pushfun v])
  generator (Number n) = do
    code %= (++ [Pushval FInteger n])
  generator (Boolean b) = do
    code %= (++ [Pushval FBool (boolToInteger b)])

{--}

{- Helper functions and generators -}
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