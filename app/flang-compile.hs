import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import SyntaxTree (Program)
import CodeGenerator (Generatable(generate))

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