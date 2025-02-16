import MainLib (flangParseAndPrint)

isDebugMode :: Bool
isDebugMode = True

main :: IO ()
main = flangParseAndPrint isDebugMode