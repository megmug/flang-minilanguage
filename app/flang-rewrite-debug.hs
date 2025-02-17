import MainLib (flangRewriteAndPrint)

isDebugMode :: Bool
isDebugMode = True

main :: IO ()
main = flangRewriteAndPrint isDebugMode