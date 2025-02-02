import MainLib (flangCompileAndPrint)

isDebugMode :: Bool
isDebugMode = True

main :: IO ()
main = flangCompileAndPrint isDebugMode