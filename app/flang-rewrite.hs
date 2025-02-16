import MainLib (flangRewriteAndPrint)

isDebugMode :: Bool
isDebugMode = False

main :: IO ()
main = flangRewriteAndPrint isDebugMode