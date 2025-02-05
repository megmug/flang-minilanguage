import HelperLib (flangTokenizeAndPrint)

isDebugMode :: Bool
isDebugMode = False

main :: IO ()
main = flangTokenizeAndPrint isDebugMode