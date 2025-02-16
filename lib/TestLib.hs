{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module TestLib where

import CodeGenerator (Code, Generatable (generate), HeapEnvironment)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (runState)
import Machine (Object (VAL), createMachineWithHeap, run)
import MachineInstruction (FType (FBool, FInteger))
import Parser (ParseResult, Parseable (parse))
import Rewriter (Rewritable (rewrite))
import SyntaxTree (Program, Stage (Core, Raw))
import Text.Parsec (ParseError)
import Tokenizer (tokenize)
import Typifier (MonoType, Typifiable (typify))

testParse :: (Parseable a) => String -> Either ParseError a
testParse prog = case tokenize prog of
  Left s -> Left s
  Right p -> parse p

testRewrite :: String -> Either String (Program Core)
testRewrite prog = case testParse prog :: ParseResult (Program Raw) of
  Left s -> Left $ show s
  Right p -> rewrite p

testTypify :: String -> Either String (Program Core, Typifier.MonoType)
testTypify prog = case testRewrite prog of
  Left s -> Left s
  Right p -> (p,) <$> typify p

testGenerate :: String -> Either String (HeapEnvironment, Code)
testGenerate prog = case testTypify prog of
  Left s -> Left $ show s
  Right (p, _) -> generate p

testRun :: String -> Either String String
testRun input = case testGenerate input of
  Left s -> Left s
  Right (h, prog) -> case createMachineWithHeap h prog of
    Nothing -> Left "machine runner crashed because of malformed machine input by code generator"
    Just m -> case runState (runExceptT run) m of
      (Left s, _) -> Left s
      (Right (VAL t v), _) -> Right $ case t of
        FInteger -> show v
        FBool -> if v == 0 then "false" else "true"
      (Right o, _) -> Left $ "Return value is malformed (" ++ show o ++ ")"