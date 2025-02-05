module HelperLib where

import CodeGenerator
  ( Code,
    HeapEnvironment,
    generate,
  )
import Control.Monad (when)
import Control.Monad.Extra (whileM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (StateT (runStateT), get, runState)
import Machine
  ( Computation,
    Object (VAL),
    createMachineWithHeap,
    getObject,
    isNotHalted,
    pop,
    prettyPrintMachineState,
    run,
    step,
  )
import MachineInstruction (FType (FBool, FInteger), Instruction)
import Parser (ParseResult, Parseable, parse)
import SyntaxTree (Program)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Parsec (ParseError)
import Token (TokenPos)
import Tokenizer (tokenize)

{- This library features helper function and IO actions that aid in in code deduplication when utilizing the compiler libraries in the executables or for testing
 - The IO actions are mainly used for the flang-*-executables
 - The test*-helpers are only used for testing, to facilitate much more succinct test cases
 -}

-- this is a helper IO action for sharing code in the different main executables
-- it reads a file and tokenizes the input string, printing debugging information if needed
tokenizeFile :: Bool -> String -> IO [TokenPos]
tokenizeFile isDebugMode path = do
  input <- readFile path
  case tokenize input of
    Left e -> do
      fail $ "Lexical error: " ++ show e
    Right ts -> do
      when isDebugMode $ do
        putStrLn "Successfully tokenized program:"
        print (map fst ts)
      return ts

-- this is a helper IO action to share code in the different main executables
-- it parses TokenPos streams into SyntaxTrees, outputting debugging information if requested
parseTokens :: Bool -> [TokenPos] -> IO Program
parseTokens isDebugMode ts = do
  case Parser.parse ts of
    Left e -> fail $ "Parse error: " ++ show e
    Right ast -> do
      when isDebugMode $ do
        putStrLn ""
        putStrLn "Successfully parsed tokens:"
        print ast
      return ast

-- This is a helper IO action to share code in the different main executables
-- it generates code for a given program syntax tree, optionally outputting debugging information
generateProgram :: Bool -> Program -> IO (HeapEnvironment, Code)
generateProgram isDebugMode ast = do
  case generate ast of
    Left err -> fail err
    Right (heap, prog) -> do
      when isDebugMode $ do
        putStrLn ""
        putStrLn "Successfully compiled program:"
        putStrLn "Heap environment:"
        print heap
        putStrLn "Code:"
        print prog
      return (heap, prog)

runIO :: Bool -> Computation IO Object
runIO isDebugMode = do
  whileM $ do
    step
    when isDebugMode $ do
      m <- lift get
      liftIO $ putStrLn ""
      liftIO $ putStrLn $ "Machine state:\n" ++ prettyPrintMachineState m ++ "\n"
    isNotHalted
  a <- pop
  getObject a

runProgramIO :: Bool -> [Object] -> [Instruction] -> IO ()
runProgramIO isDebugMode h prog = case createMachineWithHeap h prog of
  Nothing -> putStrLn "Error running program: Invalid machine code!"
  Just m -> do
    res <- runStateT (runExceptT (runIO isDebugMode)) m
    case res of
      (Left s, _) -> putStr $ "Error running program: " ++ s
      (Right (VAL t v), _) -> case t of
        FInteger -> print v
        FBool -> putStr $ if v == 0 then "False" else "True"
      (Right o, _) -> putStr $ "Return value is malformed (" ++ show o ++ ")"

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