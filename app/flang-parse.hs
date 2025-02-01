import Parser (ParseResult, parse)
import SyntaxTree (Program)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Tokenizer (tokenize)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  input <- readFile (head args)
  case tokenize input of
    Left e -> putStrLn $ "Lexical error: " ++ show e
    Right ts -> case parse ts :: ParseResult Program of
      Left e -> putStrLn $ "Parse error: " ++ show e
      Right ast -> print ast