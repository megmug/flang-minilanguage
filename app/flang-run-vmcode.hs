import Machine (Object, runProgram)
import MachineInstruction (Instruction (..))
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  input <- readFile (head args)
  let (heap, prog) = read input :: ([Object], [Instruction])
  putStrLn $ runProgram heap prog