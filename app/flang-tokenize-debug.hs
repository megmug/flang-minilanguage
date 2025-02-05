import HelperLib (flangTokenizeAndPrint)

isDebugMode :: Bool
isDebugMode = True

main :: IO ()
main = flangTokenizeAndPrint isDebugMode