import Machine (Object (..), run, createMachineWithHeap)
import MachineInstruction (Instruction (..), FType (..))
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Control.Monad.Trans.State (runState)
import Control.Monad.Trans.Except (runExceptT)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  input <- readFile (head args)
  let (heap, prog) = read input :: ([(Object)], [Instruction])
  putStrLn $ runProgram heap prog

runProgram :: [Object] -> [Instruction] -> String
runProgram h prog = case createMachineWithHeap h prog of
  Nothing -> "Invalid machine code"
  Just m -> case runState (runExceptT run) m of
    (Left msg, _) -> msg
    (Right (VAL t v), _) -> case t of
      FInteger -> show v
      FBool -> if v == 0 then "False" else "True"
    (Right o, _) -> "Return value is malformed (" ++ show o ++ ")"