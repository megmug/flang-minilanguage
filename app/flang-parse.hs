import MainLib (flangParseAndPrint)

isDebugMode :: Bool
isDebugMode = False

main :: IO ()
main = flangParseAndPrint isDebugMode