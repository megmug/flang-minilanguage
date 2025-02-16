import MainLib (flangRunVMCode)

isDebugMode :: Bool
isDebugMode = True

main :: IO ()
main = flangRunVMCode isDebugMode