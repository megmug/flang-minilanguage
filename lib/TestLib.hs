{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module TestLib where

import CodeGenerator (Code, Generatable (generate), HeapEnvironment)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (runState)
import Machine (Object (VAL), createMachineWithHeap, run)
import Parser (Parseable (parse))
import Rewriter (Rewritable (rewrite))
import SyntaxTree (Program, Stage (Core, Raw))
import Text.Parsec (ParseError)
import Tokenizer (tokenize)
import Typifier (MonoType (FBool, FInteger), Typifiable (typify))

testParse :: (Parseable a) => String -> Either ParseError a
testParse prog = case tokenize prog of
  Left s -> Left s
  Right p -> parse p

testTypify :: String -> Either String (Program Raw, MonoType)
testTypify prog = case testParse prog of
  Left s -> Left $ show s
  Right p -> (p,) <$> typify p

testRewrite :: String -> Either String (Program Core, MonoType)
testRewrite prog = case testTypify prog of
  Left s -> Left $ show s
  Right (p, t) -> (,t) <$> rewrite p

testGenerate :: String -> Either String (HeapEnvironment, Code, MonoType)
testGenerate prog = case testRewrite prog of
  Left s -> Left $ show s
  Right (p, t) -> toTripleFromTuple <$> generate p
    where
      toTripleFromTuple (a, b) = (a, b, t)

testRun :: String -> Either String String
testRun input = case testGenerate input of
  Left s -> Left s
  Right (h, prog, t) -> case createMachineWithHeap h prog of
    Nothing -> Left "machine runner crashed because of malformed machine input by code generator"
    Just m -> case runState (runExceptT run) m of
      (Left s, _) -> Left s
      (Right (VAL v), _) -> case t of
        FInteger -> Right $ show v
        FBool -> Right $ if v == 0 then "false" else "true"
        _ -> Left "expected return type is invalid"
      (Right o, _) -> Left $ "Return value is malformed (" ++ show o ++ ")"