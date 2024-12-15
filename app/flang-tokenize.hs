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
    Right (ts) -> putStrLn $ "Successfully tokenized program: " ++ (show (map fst ts))