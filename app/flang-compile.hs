import HelperLib (flangCompileAndPrint)

isDebugMode :: Bool
isDebugMode = False

main :: IO ()
main = flangCompileAndPrint isDebugMode