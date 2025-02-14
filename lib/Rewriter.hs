{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Rewriter where

import Control.Lens (use, (%=))
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, evalState)
import Data.List (intersect, (\\))
import SyntaxTree
  ( Definition (Definition),
    Expression (..),
    LocalDefinition (LocalDefinition),
    PrettyPrintable (prettyPrint),
    Program (Program),
    Stage (..),
    VariableName,
    boundByTopLevelVars,
    boundVariables,
    coreToRaw,
    freeVariables,
    hasConflictingLetBindings,
    substitute,
    substituteDefs,
    topologicallySort,
  )

newtype RewriterState = RewriterState GlobalFunctionList

type GlobalFunctionList = [VariableName]

-- Here we define a type for monadic actions that represent the types of our code generators
type Rewriter a = ExceptT String (State RewriterState) a

globalFuncs :: (Functor f) => (GlobalFunctionList -> f GlobalFunctionList) -> RewriterState -> f RewriterState
globalFuncs f (RewriterState funcs) = (\funcs' -> RewriterState funcs') <$> f funcs

class Rewritable a b where
  rewriter :: a -> Rewriter b

  -- This runs a generator with some supplied state (can also be useful for testing)
  customRewrite :: a -> RewriterState -> Either String b
  customRewrite e = evalState (runExceptT rewriteAndReturn)
    where
      rewriteAndReturn = rewriter e

  rewrite :: a -> Either String b
  rewrite e = customRewrite e (RewriterState [])

instance Rewritable (Program Raw) (Program Core) where
  rewriter (Program defs) = do
    defs' <- traverse rewriter defs
    return $ Program $ concat defs'

instance Rewritable (Definition Raw) [Definition Core] where
  rewriter (Definition f params e) = do
    (e', defs) <- rewriter e
    return $ Definition f params e' : defs

instance Rewritable (Expression Raw) (Expression Core, [Definition Core]) where
  rewriter (Let defs e) = do
    -- if there are conflicting definitions, throw error
    when (hasConflictingLetBindings defs) $ throwError $ "conflicting bindings detected in " ++ prettyPrint defs ++ "!"
    -- eliminate possible inner let-definitions in e
    (e', eDefs) <- rewriter e :: Rewriter (Expression Core, [Definition Core])
    -- now, create global function definitions and rewrite e' such that the let definitions can be eliminated
    case topologicallySort defs of
      -- recursive binding detected
      Left s -> throwError s
      Right [] -> return (e', eDefs)
      -- otherwise, we can rewrite to eliminate the bindings
      Right (def@(LocalDefinition v bindingExpr) : sortedDefs) -> do
        (bindingExpr', bindingDefs) <- rewriter bindingExpr
        gFuncNames <- use globalFuncs
        let freeVarsInDef = freeVariables e' \\ boundByTopLevelVars (def : sortedDefs)
        let paramsInNewFuncDef = freeVarsInDef `intersect` freeVariables bindingExpr'
        let calculateNewGlobalFuncName x = if x `elem` (gFuncNames ++ boundVariables e' ++ freeVariables e') then calculateNewGlobalFuncName $ x ++ "'" else x
        let v' = calculateNewGlobalFuncName v
        let calculateReplacingApp [] = Variable v'; calculateReplacingApp (x : xs) = Application (calculateReplacingApp xs) (Variable x)
        let replacingApp = calculateReplacingApp (reverse paramsInNewFuncDef) :: Expression Core
        let newFuncDef = Definition v' paramsInNewFuncDef (substitute v replacingApp bindingExpr')
        globalFuncs %= (++ [v'])
        let substitutedDefs = substituteDefs sortedDefs v (coreToRaw replacingApp)
        let substitutedInnerExpr = substitute v replacingApp e'
        let newLetExpression = Let substitutedDefs (coreToRaw substitutedInnerExpr)
        (newE, newDefs) <- rewriter newLetExpression
        return (newE, newDefs ++ bindingDefs ++ [newFuncDef] ++ eDefs)
  rewriter (IfThenElse cond e1 e2) = do
    (cond', defsCond) <- rewriter cond
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (IfThenElse cond' e1' e2', defsCond ++ defs1 ++ defs2)
  -- eliminate disjunctions by leveraging de-morgan-rule
  rewriter (Disjunction e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (LogicalNegation $ Conjunction (LogicalNegation e1') (LogicalNegation e2'), defs1 ++ defs2)
  rewriter (Conjunction e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (Conjunction e1' e2', defs1 ++ defs2)
  rewriter (LogicalNegation e) = do
    (e', defs) <- rewriter e
    return (LogicalNegation e', defs)
  rewriter (Smaller e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (Smaller e1' e2', defs1 ++ defs2)
  rewriter (Equality e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (Equality e1' e2', defs1 ++ defs2)
  -- eliminate "minus e" by using "0 - e"
  rewriter (Minus e) = do
    (e', defs) <- rewriter e
    return (Difference (Number 0) e', defs)
  rewriter (Difference e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (Difference e1' e2', defs1 ++ defs2)
  rewriter (Sum e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (Sum e1' e2', defs1 ++ defs2)
  rewriter (Quotient e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (Quotient e1' e2', defs1 ++ defs2)
  rewriter (Product e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (Product e1' e2', defs1 ++ defs2)
  rewriter (Application e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (Application e1' e2', defs1 ++ defs2)
  rewriter (Variable v) = return (Variable v, [])
  rewriter (Number n) = return (Number n, [])
  rewriter (Boolean b) = return (Boolean b, [])

{- Helper functions and rewriters -}
throwError :: String -> Rewriter a
throwError s = throwE $ "Error during rewriting: " ++ s