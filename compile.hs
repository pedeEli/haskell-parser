import System.Process
import System.Directory (removeFile)


main :: IO ()
main = do
    handle <- runCommand "ghc HaskellParser.hs"
    waitForProcess handle
    removeFile "HaskellParser.o"
    removeFile "HaskellParser.hi"
    putStrLn "compiled HaskellParser.hs"