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
import Data.List ((\\))
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
  rewriter (Program programDefs) = do
    let predefinedMultOperation = [Definition "#m" ["a", "b"] (IfThenElse (Smaller (Variable "b") (Number 0)) (Minus (Application (Application (Variable "#m") (Variable "a")) (Minus (Variable "b")))) (IfThenElse (Smaller (Number 0) (Variable "b")) (Sum (Variable "a") (Application (Application (Variable "#m") (Variable "a")) (Difference (Variable "b") (Number 1)))) (Number 0)))]
    let predefinedDivOperation = [Definition "#d" ["a", "b"] (Product (IfThenElse (LogicalNegation (Equality (Smaller (Variable "a") (Number 0)) (Smaller (Variable "b") (Number 0)))) (Minus (Number 1)) (Number 1)) (Application (Application (Variable "#dh") (IfThenElse (Smaller (Variable "a") (Number 0)) (Minus (Variable "a")) (Variable "a"))) (IfThenElse (Smaller (Variable "b") (Number 0)) (Minus (Variable "b")) (Variable "b")))), Definition "#dh" ["a", "b"] (IfThenElse (Smaller (Variable "a") (Variable "b")) (Number 0) (Sum (Application (Application (Variable "#e") (Number 2)) (Application (Application (Application (Variable "#f") (Variable "a")) (Variable "b")) (Number 0))) (Application (Application (Variable "#dh") (Difference (Variable "a") (Product (Variable "b") (Application (Application (Variable "#e") (Number 2)) (Application (Application (Application (Variable "#f") (Variable "a")) (Variable "b")) (Number 0)))))) (Variable "b")))), Definition "#f" ["a", "b", "s"] (IfThenElse (Smaller (Variable "a") (Product (Variable "b") (Application (Application (Variable "#e") (Number 2)) (Sum (Variable "s") (Number 1))))) (Variable "s") (Application (Application (Application (Variable "#f") (Variable "a")) (Variable "b")) (Sum (Variable "s") (Number 1)))), Definition "#e" ["x", "y"] (IfThenElse (Equality (Variable "y") (Number 0)) (Number 1) (Product (Variable "x") (Application (Application (Variable "#e") (Variable "x")) (Difference (Variable "y") (Number 1)))))]
    let allDefs = programDefs ++ predefinedMultOperation ++ predefinedDivOperation
    rewrittenDefs <- traverse rewriter allDefs
    return $ Program $ concat rewrittenDefs

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
        -- candidates for the parameters that should appear in the new function definition are any variables that are free in bindingExpr
        let paramCandidates = freeVariables bindingExpr
        -- we take away from that any variables that are either bound by one of the other definitions in the let, or are already known global function definitions
        let leftoverParamCandidates = paramCandidates \\ (boundByTopLevelVars (def : sortedDefs) ++ gFuncNames)
        -- now we create the application that will replace any reference to v in the other definitions, and the inner expression e'
        let calculateNewGlobalFuncName x = if x `elem` (gFuncNames ++ boundVariables e' ++ freeVariables e') then calculateNewGlobalFuncName $ x ++ "'" else x
        let v' = calculateNewGlobalFuncName v
        let calculateReplacingApp [] = Variable v'; calculateReplacingApp (x : xs) = Application (calculateReplacingApp xs) (Variable x)
        let replacingApp = calculateReplacingApp (reverse leftoverParamCandidates) :: Expression Core
        -- and here we create the new global definition replacing the let v = bindingExpr, as well as add that definition to the global function list
        let newFuncDef = Definition v' leftoverParamCandidates (substitute v replacingApp bindingExpr')
        globalFuncs %= (++ [v'])
        -- and here we actually substitute v by the new replacingApp in all other Defs and the inner expression e'
        let substitutedDefs = substituteDefs sortedDefs v (coreToRaw replacingApp)
        let substitutedInnerExpr = substitute v replacingApp e'
        -- here we assemble a new let expression with the one definition eliminated, and rewrite that one too, to eliminate all other definitions
        let newLetExpression = Let substitutedDefs (coreToRaw substitutedInnerExpr)
        (newE, newDefs) <- rewriter newLetExpression
        -- finally we return our new rewritten expression together with the resulting global definitions
        let resultingDefs = newDefs ++ bindingDefs ++ [newFuncDef] ++ eDefs
        return (newE, resultingDefs)
  rewriter (IfThenElse cond e1 e2) = do
    (cond', defsCond) <- rewriter cond
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (IfThenElse cond' e1' e2', defsCond ++ defs1 ++ defs2)
  -- eliminate disjunctions by leveraging de-morgan-rule
  rewriter (Disjunction e1 e2) = rewriter (LogicalNegation $ Conjunction (LogicalNegation e1) (LogicalNegation e2))
  -- replace "a & b" by "if a < 1 then 0 else if b < 1 then 0 else 1"
  rewriter (Conjunction e1 e2) = rewriter (IfThenElse (Smaller e1 (Number 1)) (Number 0) (IfThenElse (Smaller e2 (Number 1)) (Number 0) (Number 1)))
  -- replace logical negation by "< 1"
  rewriter (LogicalNegation e) = rewriter (Smaller e (Number 1))
  rewriter (Smaller e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (Smaller e1' e2', defs1 ++ defs2)
  -- e1 == e2 <=> (not (e1 < e2)) & (not (e2 < e1))
  rewriter (Equality e1 e2) = rewriter $ Conjunction (LogicalNegation (Smaller e1 e2)) (LogicalNegation (Smaller e2 e1))
  -- -e == 0 - e
  rewriter (Minus e) = rewriter (Difference (Number 0) e)
  rewriter (Difference e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (Difference e1' e2', defs1 ++ defs2)
  -- a + b == a - (- b)
  rewriter (Sum e1 e2) = rewriter (Difference e1 (Minus e2))
  {- replace quotients by the program
   -     #d a b = (if not ((a < 0) == (b < 0)) then -1 else 1) * #dh (if a < 0 then -a else a) (if b < 0 then -b else b);
   -
   -     #dh a b =
   -         if a < b
   -         then 0
   -         else e 2 (f a b 0) + #dh
   -     (a - (b * (#e 2 (#f a b 0)))) b;
   -
   -     #f a b s =
   -         if a < b * (#e 2 (s + 1))
   -         then s
   -         else #f a b (s + 1);
   -
   -     #e x y = if y == 0 then 1 else x * #e x (y - 1);
   - that recursively calculates the integer division
   -}
  rewriter (Quotient e1 e2) = rewriter (Application (Application (Variable "#d") e1) e2)
  {- if replaceMult is True, then it is implemented as the following combinator:
   - #m a b = if b < 0 then - (#m a (-b)) else if 0 < b then a + #m a (b - 1) else 0;
   -}
  rewriter (Product e1 e2) = rewriter (Application (Application (Variable "#m") e1) e2)
  rewriter (Application e1 e2) = do
    (e1', defs1) <- rewriter e1
    (e2', defs2) <- rewriter e2
    return (Application e1' e2', defs1 ++ defs2)
  rewriter (Variable v) = return (Variable v, [])
  rewriter (Number n) = return (Number n, [])
  -- replace boolean value: True -> 1, False -> 0
  rewriter (Boolean b) = return (Number $ if b then 1 else 0, [])

{- Helper functions and rewriters -}
throwError :: String -> Rewriter a
throwError s = throwE $ "Error during rewriting: " ++ s