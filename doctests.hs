import Test.DocTest (doctest)
import System.FilePath.Glob (globDir, compile)
import Data.List (isSuffixOf)

main = do
    (_, paths) <- globDir [compile "*.hs"] "src"
    doctest $ ["-isrc"] ++ (filter (isSuffixOf ".hs") paths)
