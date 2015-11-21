module Tpll.Parser
(
    parseString
) where


import Tpll.Tokenizer (Token(Tag, Variable, Text, content, line), tokenize)
import Tpll.Context (Context, ctx)
import Tpll.Tags (Tag, Tags, TagResult, tags)
import Tpll.Tags.Default (firstOfTag, nowTag)


import Prelude hiding (lookup)
import Control.Monad (liftM2)
import Data.Map.Strict (lookup)


-- | Consume tag token
--
-- Examples:
--
-- >>> let ctx' = ctx [("a", ""), ("b", "x")]
-- >>> let tags' = tags [("firstof", firstOfTag)]
-- >>> let (_, r) = consumeTag ctx' tags' (Tag { content = "firstof a b", line = 1 }) []
-- >>> r
-- "x"
-- >>> let (_, r) = consumeTag ctx' tags' (Tag { content = "x", line = 1 }) []
-- >>> r
-- "<error>"
consumeTag :: Context -> Tags -> Token -> [Token] -> TagResult
consumeTag ctx' tags' token tokens =
    let Tag { content = content, line = _ } = token
        ([name], parts) = splitAt 1 $ words content
    in
        case lookup name tags' of
            Just fn -> fn ctx' token tokens
            Nothing ->
                (tokens, return "<error>")


-- | Parse tokens
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", "bar"), ("bar", show 2)]
-- >>> let tags' = tags []
-- >>> lookup "foo" ctx'
-- Just "bar"
-- >>> let [r] = parseTokens ctx' tags' [Variable { content = "foo", line = 1 }]
-- >>> r
-- "bar"
-- >>> let [r] = parseTokens ctx' tags' [Variable { content = "unknown", line = 1 }]
-- >>> r
-- ""
parseTokens :: Context -> Tags -> [Token] -> [IO String]
parseTokens ctx' tags' tokens =
    parseTokens' ctx' tags' tokens []

parseTokens' :: Context -> Tags -> [Token] -> [IO String] -> [IO String]
parseTokens' _ _ [] acc = reverse acc
parseTokens' ctx' tags' tokens acc =
    let (newtokens, result) = parseToken ctx' tags' tokens
    in
        parseTokens' ctx' tags' newtokens (result:acc)


-- | Parse token
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", "foo")]
-- >>> let tags' = tags []
-- >>> let (_, r) = parseToken ctx' tags' [Variable { content = "foo", line = 1 }]
-- >>> r
-- "foo"
parseToken :: Context -> Tags -> [Token] -> TagResult
parseToken ctx' tags' (token:tokens) =
    case token of
        Variable { content = content, line = _ } ->
            case lookup content ctx' of
                Just a -> (tokens, return a)
                _ -> (tokens, return "")
        Text { content = content, line = _ } ->
            (tokens, return content)
        Tag { content = _, line = _ } ->
            consumeTag ctx' tags' token tokens
        _ ->
            (tokens, return "")


-- | Parse string
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", "bar"), ("bar", show 2), ("bang", "")]
-- >>> let tags' = tags [("firstof", firstOfTag)]
-- >>> parseString ctx' tags' "abc{{ foo }}def{{ bar }}x"
-- "abcbardef2x"
--
-- >>> parseString ctx' tags' "foo{% firstof bang %}bar"
-- "foobar"
--
-- >>> parseString ctx' tags' "foo{% firstof bang bar %}bar"
-- "foo2bar"
parseString :: Context -> Tags -> String -> IO String
parseString ctx' tags' text =
    foldl1 (liftM2 (++)) $ parseTokens ctx' tags' $ tokenize text
