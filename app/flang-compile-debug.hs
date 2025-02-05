import HelperLib (flangCompileAndPrint)

isDebugMode :: Bool
isDebugMode = True

main :: IO ()
main = flangCompileAndPrint isDebugMode