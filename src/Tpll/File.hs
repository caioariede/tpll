{-|
Module      : Tpll.File
Description : Functions to parse files
-}

module Tpll.File
(
)
where


import Tpll.Context (ctx, Context, cStr)
import Tpll.Tags (Tags)
import Tpll.Tags.Default (getAllDefaultTags)
import Tpll.Parser (parseString)


-- | Render a file
--
-- Examples:
--
-- >>> let ctx' = ctx [("a", cStr ""), ("b", cStr "42"), ("c", cStr "")]
-- >>> let tags' = getAllDefaultTags
-- >>> renderFile "misc/index.html" ctx' tags'
-- "42\n"
renderFile :: String -> Context -> Tags -> IO String
renderFile path ctx' tags' = do
    content <- readFile path
    parseString ctx' tags' content
