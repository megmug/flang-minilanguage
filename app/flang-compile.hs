import CodeGenerator (Generatable (generate))
import SyntaxTree (Program)
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

{- For now, this is compiling straight from syntax trees until we have a parser -}
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  input <- readFile (head args)
  let st = read input :: Program
  case generate st of
    Left s -> putStrLn s
    Right pr -> print pr