import HelperLib (flangRunVMCode)

isDebugMode :: Bool
isDebugMode = True

main :: IO ()
main = flangRunVMCode isDebugMode