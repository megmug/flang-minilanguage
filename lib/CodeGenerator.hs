{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use record patterns" #-}
module CodeGenerator where

import Control.Lens (use)
import Control.Lens.Operators ((%=), (.=))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState, get)
import Data.List (delete, (\\))
import Data.List.Index (indexed)
import Machine (Object (DEF), boolToInteger)
import MachineInstruction
  ( Arity,
    FOperator
      ( And,
        Divide,
        Equals,
        FIf,
        Minus,
        Not,
        Or,
        Plus,
        Smaller,
        Times
      ),
    FType (FBool, FInteger),
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
    OperatorArg (One, OpIf, Two),
    UpdateArg (Arity, PredefinedOperator),
  )
import SyntaxTree
  ( Definition (..),
    Expression (..),
    LocalDefinition (LocalDefinition),
    Program (..),
    VariableName,
    boundByTopLevelVars,
    boundVariables,
    freeVariables,
    isIndependentFrom,
    substitute,
    substituteDefs,
  )

{- The code generator maintains a state that carries:
    - the current prefix length (how many machine instructions precede the one that will be generated next?)
    - a position list that specifies a mapping of names to their respective positions in the parameter list
    - the code that was already generated, maintained as a simple list
    - a heap environment that is used to store the generated DEF function definitions that the machine requires for operation
 -}
data GenState = GenState PosList Code HeapEnvironment AccumulatedFunctionDefinitions GlobalFunctionList

type PrefixLength = Int

type PosList = [(Int, VariableName)]

type Code = [Instruction]

type HeapEnvironment = [Object]

type AccumulatedFunctionDefinitions = [Definition]

type GlobalFunctionList = [(VariableName, Arity)]

-- Here we define a type for monadic actions that represent the types of our code generators
type Generator a = ExceptT String (State GenState) a

{- Lens definitions for GenState -}
posList :: (Functor f) => (PosList -> f PosList) -> GenState -> f GenState
posList f (GenState pos c h acc funcs) = (\pos' -> GenState pos' c h acc funcs) <$> f pos

code :: (Functor f) => (Code -> f Code) -> GenState -> f GenState
code f (GenState pos c h acc funcs) = (\code' -> GenState pos code' h acc funcs) <$> f c

heapEnv :: (Functor f) => (HeapEnvironment -> f HeapEnvironment) -> GenState -> f GenState
heapEnv f (GenState pos c h acc funcs) = (\h' -> GenState pos c h' acc funcs) <$> f h

accDefinitions :: (Functor f) => (AccumulatedFunctionDefinitions -> f AccumulatedFunctionDefinitions) -> GenState -> f GenState
accDefinitions f (GenState pos c h acc funcs) = (\acc' -> GenState pos c h acc' funcs) <$> f acc

globalFuncs :: (Functor f) => (GlobalFunctionList -> f GlobalFunctionList) -> GenState -> f GenState
globalFuncs f (GenState pos c h acc funcs) = (\funcs' -> GenState pos c h acc funcs') <$> f funcs

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
        (GenState _ c h _ _) <- lift get
        return (h, c)

  -- This runs a generator with some default empty state (mostly useful for whole programs)
  generate :: a -> Either String (HeapEnvironment, Code)
  generate e = customGenerate e $ GenState [] [] [] [] []

{--}

{- Class instances -}
{- Convention: Every generator ensures to clean up its state after itself -}
instance Generatable Program where
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
           Return,
           -- subroutine for unary operator (address: 20)
           Pushparam 2,
           Unwind,
           Call,
           Operator One,
           Update PredefinedOperator,
           Return
         ]
    -- to understand why we need to generate function defs iteratively, see generateDefs
    accDefinitions .= defs
    generateDefs

instance Generatable Definition where
  generator def@(Definition f params e) = do
    c <- use code
    let addr = length c
    let n = length params
    -- add new function DEF to heap environment
    heapEnv %= (++ [DEF f n addr])
    -- add new function to global environment
    globalFuncs %= (++ [(f, length params)])
    -- add params to posList
    posList .= paramsToPosList params
    -- generate defining expression
    generator e
    -- append suffix for cleanup (at runtime) and returning
    code %= (++ [Update (Arity n), Slide (n + 1), Unwind, Call, Return])
    -- reset posList (compile-time cleanup)
    posList .= []
    -- remove function def from accumulated definitions to avoid generating it again (necessary because of iterative generation of function defs, see generateDefs)
    accDefinitions %= delete def

instance Generatable Expression where
  generator (Let [] e) = generator e
  generator expr@(Let (def@(LocalDefinition v e) : defs) e') = do
    if def `isIndependentFrom` defs
      then do
        gFuncs <- use globalFuncs
        let gFuncNames = map fst gFuncs
        let freeVarsInDef = freeVariables e \\ boundByTopLevelVars (def : defs)
        let calculateNewGlobalFuncName x = if x `elem` (gFuncNames ++ boundVariables expr ++ freeVariables expr) then calculateNewGlobalFuncName $ x ++ "'" else x
        let v' = calculateNewGlobalFuncName v
        let calculateReplacingApp [] = Variable v'; calculateReplacingApp (x : xs) = Application (calculateReplacingApp xs) (Variable x)
        let replacingApp = calculateReplacingApp (reverse freeVarsInDef)
        let newFuncDef = Definition v' freeVarsInDef (substitute v replacingApp e)
        let newGlobalFuncEntry = (v', length freeVarsInDef)
        accDefinitions %= (++ [newFuncDef])
        globalFuncs %= (++ [newGlobalFuncEntry])
        let substitutedDefs = substituteDefs defs v replacingApp
        let substitutedE' = substitute v replacingApp e'
        let newLetExpression = Let substitutedDefs substitutedE'
        generator newLetExpression
      else do
        -- if first def is dependent on other defs, just cycle through until an independent one is found
        -- in case of recursive definitions, this will loop forever, but those are unsupported
        generator (Let (defs ++ [def]) e')
  generator (IfThenElse e1 e2 e3) = do
    generator e3
    posList %= posPlus 1
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre FIf, Makeapp, Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-2)
  -- NOTE: the disjunction or conjunction rule can be optimized by leveraging de-morgan-rule in syntax tree
  generator (Disjunction e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre Or, Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-1)
  generator (Conjunction e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre And, Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-1)
  generator (LogicalNegation e) = do
    generator e
    code %= (++ [Pushpre Not, Makeapp])
  generator (SyntaxTree.Smaller e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre MachineInstruction.Smaller, Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-1)
  generator (Equality e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre Equals, Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-1)
  -- NOTE: the difference operation can be replaced by sum + minus in syntax tree. In this case, we need to implement the unary - differently
  generator (SyntaxTree.Minus e) = generator (Difference (Number 0) e)
  generator (Difference e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre MachineInstruction.Minus, Makeapp, Makeapp])
    -- cleanup - see Application rule
    posList %= posPlus (-1)
  generator (Sum e1 e2) = do
    generator e2
    posList %= posPlus 1
    generator e1
    code %= (++ [Pushpre Plus, Makeapp, Makeapp])
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
      Nothing -> code %= (++ [Pushfun v])
  generator (Number n) = do
    code %= (++ [Pushval FInteger n])
  generator (Boolean b) = do
    code %= (++ [Pushval FBool (boolToInteger b)])

{--}

{- Helper functions and generators -}

-- Iteratively generate global function definitions
-- We need to do this because a function definition can arbitrarily yield more definitions in the form of local let definitions - so we iterate until the accumulated definitions are empty
generateDefs :: Generator ()
generateDefs = do
  acc <- use accDefinitions
  case acc of
    [] -> return ()
    def : _ -> do
      generator def
      generateDefs

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