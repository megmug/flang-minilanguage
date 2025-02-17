{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module MainLib where

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
import Control.Monad.Trans.State (StateT (runStateT), get)
import Machine
  ( Computation,
    Object (VAL),
    createMachineWithHeap,
    getObject,
    isNotHalted,
    pop,
    prettyPrintMachineState,
    step,
  )
import MachineInstruction (Instruction)
import Parser (parse)
import Rewriter (Rewritable (rewrite))
import SyntaxTree (Program, Stage (..), prettyPrint)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Token (TokenPos)
import Tokenizer (tokenize)
import Typifier (MonoType (FBool, FInteger), Typifiable (typify))

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
parseTokens :: Bool -> [TokenPos] -> IO (Program Raw)
parseTokens isDebugMode ts = do
  case Parser.parse ts of
    Left e -> fail $ "Parse error: " ++ show e
    Right ast -> do
      when isDebugMode $ do
        putStrLn ""
        putStrLn "Successfully parsed tokens:"
        print ast
      return ast

typifyProgram :: Bool -> Program Raw -> IO (Program Raw, MonoType)
typifyProgram isDebugMode prog = do
  case typify prog of
    Left err -> fail err
    Right t -> do
      when isDebugMode $ do
        putStrLn ""
        putStrLn "Successfully typified program as:"
        print t
      return (prog, t)

rewriteProgram :: Bool -> Program Raw -> MonoType -> IO (Program Core, MonoType)
rewriteProgram isDebugMode prog t = do
  case rewrite prog of
    Left err -> fail err
    Right prog' -> do
      when isDebugMode $ do
        putStrLn ""
        putStrLn "Successfully rewritten program:"
        print prog'
      return (prog', t)

-- This is a helper IO action to share code in the different main executables
-- it generates code for a given program syntax tree, optionally outputting debugging information
generateProgram :: Bool -> Program Core -> MonoType -> IO (HeapEnvironment, Code, MonoType)
generateProgram isDebugMode ast t = do
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
      return (heap, prog, t)

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

runProgramIO :: Bool -> [Object] -> [Instruction] -> MonoType -> IO ()
runProgramIO isDebugMode h prog t = case createMachineWithHeap h prog of
  Nothing -> putStrLn "Error running program: Invalid machine code!"
  Just m -> do
    res <- runStateT (runExceptT (runIO isDebugMode)) m
    case res of
      (Left s, _) -> putStr $ "Error running program: " ++ s
      (Right (VAL v), _) -> case t of
        FInteger -> print v
        FBool -> putStr $ if v == 0 then "false" else "true"
        _ -> putStr "expected return type is invalid"
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

flangParse :: Bool -> IO (Program Raw)
flangParse isDebugMode = do
  ts <- flangTokenize isDebugMode
  parseTokens isDebugMode ts

flangParseAndPrint :: Bool -> IO ()
flangParseAndPrint isDebugMode = do
  ast <- flangParse isDebugMode
  if isDebugMode then return () else putStrLn $ prettyPrint ast

flangTypify :: Bool -> IO (Program Raw, MonoType)
flangTypify isDebugMode = do
  prog <- flangParse isDebugMode
  typifyProgram isDebugMode prog

flangTypifyAndPrint :: Bool -> IO ()
flangTypifyAndPrint isDebugMode = do
  (_, t) <- flangTypify isDebugMode
  if isDebugMode then return () else print t

flangRewrite :: Bool -> IO (Program Core, MonoType)
flangRewrite isDebugMode = do
  (ast, t) <- flangTypify isDebugMode
  rewriteProgram isDebugMode ast t

flangRewriteAndPrint :: Bool -> IO ()
flangRewriteAndPrint isDebugMode = do
  (ast, _) <- flangRewrite isDebugMode
  if isDebugMode then return () else putStrLn $ prettyPrint ast

flangCompile :: Bool -> IO (HeapEnvironment, Code, MonoType)
flangCompile isDebugMode = do
  (ast, t) <- flangRewrite isDebugMode
  generateProgram isDebugMode ast t

flangCompileAndPrint :: Bool -> IO ()
flangCompileAndPrint isDebugMode = do
  res <- flangCompile isDebugMode
  if isDebugMode then return () else print res

flangRun :: Bool -> IO ()
flangRun isDebugMode = do
  (heap, code, t) <- flangCompile isDebugMode
  runProgramIO isDebugMode heap code t

flangRunVMCode :: Bool -> IO ()
flangRunVMCode isDebugMode = do
  args <- getArgsSetBuffering
  input <- readFile (head args)
  let (heap, prog, t) = read input :: ([Object], [Instruction], MonoType)
  runProgramIO isDebugMode heap prog t