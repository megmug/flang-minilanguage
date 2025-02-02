module MainLib where

import CodeGenerator (Code, HeapEnvironment, generateProgram)
import Machine (Object, runProgramIO)
import MachineInstruction (Instruction)
import Parser (parseTokens)
import SyntaxTree (Program)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Token (TokenPos)
import Tokenizer (tokenizeFile)

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