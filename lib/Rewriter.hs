{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Rewriter where

import Control.Lens (use, (%=), (.=))
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState)
import Data.List (delete, intersect, (\\))
import SyntaxTree (Definition (Definition), Expression (..), LocalDefinition (LocalDefinition), PrettyPrintable (prettyPrint), Program (Program), VariableName, boundByTopLevelVars, boundVariables, freeVariables, hasConflictingLetBindings, substitute, substituteDefs, topologicallySort)

data RewriterState = RewriterState AccumulatedFunctionDefinitions GlobalFunctionList

type AccumulatedFunctionDefinitions = [Definition]

type GlobalFunctionList = [VariableName]

-- Here we define a type for monadic actions that represent the types of our code generators
type Rewriter a = ExceptT String (State RewriterState) a

accDefinitions :: (Functor f) => (AccumulatedFunctionDefinitions -> f AccumulatedFunctionDefinitions) -> RewriterState -> f RewriterState
accDefinitions f (RewriterState acc funcs) = (\acc' -> RewriterState acc' funcs) <$> f acc

globalFuncs :: (Functor f) => (GlobalFunctionList -> f GlobalFunctionList) -> RewriterState -> f RewriterState
globalFuncs f (RewriterState acc funcs) = (\funcs' -> RewriterState acc funcs') <$> f funcs

class Rewritable a where
  rewriter :: a -> Rewriter a

  -- This runs a generator with some supplied state (can also be useful for testing)
  customRewrite :: a -> RewriterState -> Either String a
  customRewrite e = evalState (runExceptT rewriteAndReturn)
    where
      rewriteAndReturn = rewriter e

  rewrite :: a -> Either String a
  rewrite e = customRewrite e (RewriterState [] [])

instance Rewritable Program where
  rewriter (Program defs) = do
    accDefinitions .= defs
    Program <$> rewriteDefs

-- Iteratively generate global function definitions
-- We need to do this because a function definition can arbitrarily yield more definitions in the form of local let definitions - so we iterate until the accumulated definitions are empty
rewriteDefs :: Rewriter [Definition]
rewriteDefs = do
  acc <- use accDefinitions
  case acc of
    [] -> return []
    def : _ -> do
      d <- rewriter def
      ds <- rewriteDefs
      return (d : ds)

instance Rewritable Definition where
  rewriter def@(Definition f params e) = do
    e' <- rewriter e
    accDefinitions %= delete def
    return $ Definition f params e'

instance Rewritable Expression where
  rewriter (Let defs e) = do
    -- eliminate possible inner let-definitions in e
    e' <- rewriter e
    -- if there are conflicting definitions, throw error
    when (hasConflictingLetBindings defs) $ throwError $ "conflicting bindings detected in " ++ prettyPrint defs ++ "!"
    -- now, create global function definitions and rewrite e' such that the let definitions can be eliminated
    case topologicallySort defs of
      -- recursive binding detected
      Left s -> throwError s
      Right [] -> return e'
      -- otherwise, we can recursively rewrite to eliminate the bindings
      Right (def@(LocalDefinition v bindingExpr) : sortedDefs) -> do
        gFuncNames <- use globalFuncs
        let freeVarsInDef = freeVariables e' \\ boundByTopLevelVars (def : sortedDefs)
        let paramsInNewFuncDef = freeVarsInDef `intersect` freeVariables bindingExpr
        let calculateNewGlobalFuncName x = if x `elem` (gFuncNames ++ boundVariables e' ++ freeVariables e') then calculateNewGlobalFuncName $ x ++ "'" else x
        let v' = calculateNewGlobalFuncName v
        let calculateReplacingApp [] = Variable v'; calculateReplacingApp (x : xs) = Application (calculateReplacingApp xs) (Variable x)
        let replacingApp = calculateReplacingApp (reverse paramsInNewFuncDef)
        let newFuncDef = Definition v' paramsInNewFuncDef (substitute v replacingApp bindingExpr)
        accDefinitions %= (++ [newFuncDef])
        globalFuncs %= (++ [v'])
        let substitutedDefs = substituteDefs sortedDefs v replacingApp
        let substitutedInnerExpr = substitute v replacingApp e'
        let newLetExpression = Let substitutedDefs substitutedInnerExpr
        rewriter newLetExpression
  rewriter (IfThenElse cond e1 e2) = IfThenElse <$> rewriter cond <*> rewriter e1 <*> rewriter e2
  rewriter (Disjunction e1 e2) = LogicalNegation <$> (Conjunction <$> (LogicalNegation <$> rewriter e1) <*> (LogicalNegation <$> rewriter e2))
  rewriter (Conjunction e1 e2) = Conjunction <$> rewriter e1 <*> rewriter e2
  rewriter (LogicalNegation e) = LogicalNegation <$> rewriter e
  rewriter (Smaller e1 e2) = Smaller <$> rewriter e1 <*> rewriter e2
  rewriter (Equality e1 e2) = Equality <$> rewriter e1 <*> rewriter e2
  rewriter (Minus e) = Difference (Number 0) <$> rewriter e
  rewriter (Difference e1 e2) = Difference <$> rewriter e1 <*> rewriter e2
  rewriter (Sum e1 e2) = Sum <$> rewriter e1 <*> rewriter e2
  rewriter (Quotient e1 e2) = Quotient <$> rewriter e1 <*> rewriter e2
  rewriter (Product e1 e2) = Product <$> rewriter e1 <*> rewriter e2
  rewriter (Application e1 e2) = Application <$> rewriter e1 <*> rewriter e2
  rewriter v@(Variable _) = return v
  rewriter n@(Number _) = return n
  rewriter b@(Boolean _) = return b

{- Helper functions and rewriters -}
throwError :: String -> Rewriter a
throwError s = throwE $ "Error during rewriting: " ++ s