import HelperLib (flangRunVMCode)

isDebugMode :: Bool
isDebugMode = False

main :: IO ()
main = flangRunVMCode isDebugMode