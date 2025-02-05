module HelperLib where

import CodeGenerator (unsafeGenerate)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (runState)
import Machine (Object (VAL), createMachineWithHeap, run)
import MachineInstruction (FType (FBool, FInteger))

-- Helper function to ease implementation of readable tests
-- ONLY used in testing context
unsafeRun :: String -> Either String String
unsafeRun input = case unsafeGenerate input of
  Left _ -> error "unsafe machine runner crashed because of failed code generation"
  Right (h, prog) -> case createMachineWithHeap h prog of
    Nothing -> error "unsafe machine runner crashed because of malformed machine input by code generator"
    Just m -> case runState (runExceptT run) m of
      (Left s, _) -> Left s
      (Right (VAL t v), _) -> Right $ case t of
        FInteger -> show v
        FBool -> if v == 0 then "False" else "True"
      (Right o, _) -> Left $ "Return value is malformed (" ++ show o ++ ")"