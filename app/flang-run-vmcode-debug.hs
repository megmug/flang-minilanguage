import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (runState)
import Machine (Object, Machine (..), createMachineWithHeap, isNotHalted, prettyPrintMachineState, step)
import MachineInstruction (Instruction (..))
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  input <- readFile (head args)
  let (heap, prog) = read input :: ([Object], [Instruction])
  case createMachineWithHeap heap prog of
    Just machine@(Machine c _ _ _ _ _) -> do
      putStrLn $ "Machine code: " ++ show c
      putStrLn "Running machine now step by step:"
      run machine 0
    Nothing -> putStrLn "Malformed machine code!"

run :: Machine -> Integer -> IO ()
run m i = do
  putStrLn $ "Machine state in step " ++ show i ++ ": "
  putStrLn $ prettyPrintMachineState m ++ "\n"
  case runState (runExceptT $ do step; isNotHalted) m of
    (Left msg, _) -> putStrLn $ "Error from machine: " ++ msg
    (Right False, m') -> putStrLn $ "Machine halted in step " ++ show (i + 1) ++ " with state: " ++ prettyPrintMachineState m' ++ "\n"
    (Right True, m') -> run m' (i + 1)