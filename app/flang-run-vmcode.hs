import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import MachineInstruction (Instruction (..))
import Machine (runProgram)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  input <- readFile (head args)
  let prog = read input :: [Instruction]
  putStrLn $ runProgram prog