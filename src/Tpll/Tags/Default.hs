module Tpll.Tags.Default
(
    firstOfTag,
    nowTag
) where


import Tpll.Context (Context, ctx)
import Tpll.Tokenizer (Token(Tag, content, line))
import Tpll.Tags (TagResult)


import Prelude hiding (lookup)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Map.Strict (lookup)


-- | template tag: firstof
--
-- Examples:
--
-- >>> let ctx' = ctx [("a", ""), ("b", "x")]
-- >>> let (_, r) = firstOfTag ctx' (Tag { content = "firstof a b", line = 1 }) []
-- >>> r
-- "x"
--
-- >>> let (_, r) = firstOfTag ctx' (Tag { content = "firstof a c", line = 1 }) []
-- >>> r
-- ""
firstOfTag :: Context -> Token -> [Token] -> TagResult
firstOfTag ctx' token tokens =
    let Tag { content = content, line = _ } = token
        (_, parts) = splitAt 1 $ words content
    in
        (tokens, return $ firstOfTag' ctx' parts)

firstOfTag' :: Context -> [String] -> String
firstOfTag' _ [] = ""
firstOfTag' ctx' (x:xs) =
    case lookup x ctx' of
        Just x ->
            if x == "" then
                firstOfTag' ctx' xs
            else
                x
        Nothing ->
            firstOfTag' ctx' xs


-- | template tag: now
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> let expected = fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%I:%S") getCurrentTime
-- >>> let (_, r) = nowTag ctx' (Tag { content = "now", line = 1 }) []
-- >>> :{
--  do
--      a <- expected
--      b <- r
--      print $ show $ a == b
-- :}
-- "True"
nowTag :: Context -> Token -> [Token] -> TagResult
nowTag ctx' token tokens =
    let result = fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%I:%S") getCurrentTime
    in
        ([], result)
