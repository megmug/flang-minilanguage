module HelperLib where

import CodeGenerator
  ( Code,
    HeapEnvironment,
    generate,
    generateProgram,
  )
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (runState)
import Machine
  ( Object (VAL),
    createMachineWithHeap,
    run,
    runProgramIO,
  )
import MachineInstruction (FType (FBool, FInteger), Instruction)
import Parser (ParseResult, Parseable, parse, parseTokens)
import SyntaxTree (Program)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Parsec (ParseError)
import Token (TokenPos)
import Tokenizer (tokenize, tokenizeFile)

{- This library features helper function and IO actions that aid in in code deduplication when utilizing the compiler libraries in the executables or for testing
 - The IO actions are mainly used for the flang-*-executables
 - The test*-helpers are only used for testing, to facilitate much more succinct test cases
 -}

getArgsSetBuffering :: IO [String]
getArgsSetBuffering = do
  hSetBuffering stdout NoBuffering
  getArgs

flangTokenize :: Bool -> IO [TokenPos]
flangTokenize isDebugMode = do
  args <- getArgsSetBuffering
  tokenizeFile isDebugMode (head args)

flangTokenizeAndPrint :: Bool -> IO ()
flangTokenizeAndPrint isDebugMode = do
  ts <- flangTokenize isDebugMode
  if isDebugMode then return () else print (map fst ts)

flangParse :: Bool -> IO Program
flangParse isDebugMode = do
  ts <- flangTokenize isDebugMode
  parseTokens isDebugMode ts

flangParseAndPrint :: Bool -> IO ()
flangParseAndPrint isDebugMode = do
  ast <- flangParse isDebugMode
  if isDebugMode then return () else print ast

flangCompile :: Bool -> IO (HeapEnvironment, Code)
flangCompile isDebugMode = do
  ast <- flangParse isDebugMode
  generateProgram isDebugMode ast

flangCompileAndPrint :: Bool -> IO ()
flangCompileAndPrint isDebugMode = do
  res <- flangCompile isDebugMode
  if isDebugMode then return () else print res

flangRun :: Bool -> IO ()
flangRun isDebugMode = do
  (heap, code) <- flangCompile isDebugMode
  runProgramIO isDebugMode heap code

flangRunVMCode :: Bool -> IO ()
flangRunVMCode isDebugMode = do
  args <- getArgsSetBuffering
  input <- readFile (head args)
  let (heap, prog) = read input :: ([Object], [Instruction])
  runProgramIO isDebugMode heap prog

testParse :: (Parseable a) => String -> Either ParseError a
testParse prog = case tokenize prog of
  Left s -> Left s
  Right p -> parse p

testGenerate :: String -> Either String (HeapEnvironment, Code)
testGenerate prog = case testParse prog :: ParseResult Program of
  Left s -> Left $ show s
  Right p -> generate p

testRun :: String -> Either String String
testRun input = case testGenerate input of
  Left s -> Left s
  Right (h, prog) -> case createMachineWithHeap h prog of
    Nothing -> Left "machine runner crashed because of malformed machine input by code generator"
    Just m -> case runState (runExceptT run) m of
      (Left s, _) -> Left s
      (Right (VAL t v), _) -> Right $ case t of
        FInteger -> show v
        FBool -> if v == 0 then "False" else "True"
      (Right o, _) -> Left $ "Return value is malformed (" ++ show o ++ ")"