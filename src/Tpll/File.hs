module Tpll.File
(
)
where


import Tpll.Context (ctx, Context)
import Tpll.Tags (tags, Tags)
import Tpll.Parser (parseString)
import Tpll.Tags.Default (firstOfTag)


-- | Render a file
--
-- Examples:
--
-- >>> let ctx' = ctx [("a", ""), ("b", "42"), ("c", "")]
-- >>> let tags' = tags [("firstof", firstOfTag)]
-- >>> renderFile "misc/index.html" ctx' tags'
-- "42\n"
renderFile :: String -> Context -> Tags -> IO String
renderFile path ctx' tags' = do
    content <- readFile path
    parseString ctx' tags' content
