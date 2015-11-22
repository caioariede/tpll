module Tpll.Parser
(
    parseString,
    parseTokens
) where


import Tpll.Tokenizer (Token(Tag, Variable, Text, content, line), tokenize)
import Tpll.Context (Context, ctx, ContextValue(CStr, CInt, CList))
import Tpll.Tags (Tag, Tags, TagAction(Render, RenderBlock), tags)
import Tpll.Tags.Default (getAllDefaultTags)


import Prelude hiding (lookup)
import Control.Monad (liftM2)
import Data.Map.Strict (lookup)


-- | Consume tag token
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> let tags' = tags []
-- >>> let Render (_, r) = consumeTag ctx' tags' (Tag { content = "x", line = 1 }) []
-- >>> r
-- "<error>"
consumeTag :: Context -> Tags -> Token -> [Token] -> TagAction
consumeTag ctx' tags' token tokens =
    let Tag { content = content, line = _ } = token
        ([name], parts) = splitAt 1 $ words content
    in
        case lookup name tags' of
            Just fn ->
                fn ctx' token tokens
            Nothing ->
                Render (tokens, return "<error>")


-- | Parse tokens
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", CStr "bar"), ("bar", CStr "2")]
-- >>> let tags' = tags []
-- >>> let Just (CStr r) = lookup "foo" ctx'
-- >>> r
-- "bar"
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


renderBlock :: [Context] -> Tags -> [Token] -> Token -> [IO String] -> ([Token], IO String)
renderBlock [] _ _ _ acc =
    ([], foldl1 (liftM2 (++)) $ reverse acc)
renderBlock (ctx':ctxstack) tags' tokens untilToken acc =
    let result = parseTokensUntil ctx' tags' tokens untilToken
    in
        renderBlock ctxstack tags' tokens untilToken (result ++ acc)


-- | Parse tokens until
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", CStr "bar"), ("bar", CStr "2")]
-- >>> let tags' = tags []
-- >>> let tokens = [Variable { content = "foo", line = 1 }, Variable { content = "x", line = 1 }, Variable { content = "bar", line = 2 }]
-- >>> let r = parseTokensUntil ctx' tags' tokens (Variable { content = "x", line = 0 })
-- >>> length r
-- 1
-- >>> head r
-- "bar"
parseTokensUntil :: Context -> Tags -> [Token] -> Token -> [IO String]
parseTokensUntil ctx' tags' tokens untilToken =
    parseTokensUntil' ctx' tags' tokens untilToken []


parseTokensUntil' :: Context -> Tags -> [Token] -> Token -> [IO String] -> [IO String]
parseTokensUntil' _ _ [] untilToken acc =
    reverse acc

parseTokensUntil' ctx' tags' (token:tokens) untilToken acc =
    -- this is a bit hacky, but we need to ignore the line
    let currentToken = token { line = 0 }
        neededToken = untilToken { line = 0 }
    in
        if currentToken == neededToken then
            -- just ignore the currentToken
            acc
        else
            let (newtokens, result) = parseToken ctx' tags' ([token] ++ tokens)
            in
                parseTokensUntil' ctx' tags' newtokens untilToken (result:acc)


-- | Parse token
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", CStr "foo")]
-- >>> let tags' = tags []
-- >>> let (_, r) = parseToken ctx' tags' [Variable { content = "foo", line = 1 }]
-- >>> r
-- "foo"
parseToken :: Context -> Tags -> [Token] -> ([Token], IO String)
parseToken ctx' tags' (token:tokens) =
    case token of
        Variable { content = content, line = _ } ->
            case lookup content ctx' of
                Just (CStr a) ->
                    (tokens, return a)
                Just (CInt a) ->
                    (tokens, return $ show a)
                _ ->
                    (tokens, return "")
        Text { content = content, line = _ } ->
            (tokens, return content)
        Tag { content = _, line = _ } ->
            let result = consumeTag ctx' tags' token tokens
            in
                case result of
                    RenderBlock (ctxstack, oldctx, newtokens, untilToken) ->
                        renderBlock ctxstack tags' newtokens untilToken []
                    Render (newtokens, render) ->
                        (newtokens, render)
        _ ->
            (tokens, return "")


-- | Parse string
--
-- Examples:
--
-- 
-- >>> let ctx' = ctx [("bang", CStr ""), ("bar", CInt 2)]
-- >>> parseString ctx' getAllDefaultTags "foo{% firstof bang %}bar"
-- "foobar"
--
-- >>> parseString ctx' getAllDefaultTags "foo{% firstof bang bar %}bar"
-- "foo2bar"
--
-- >>> let ctx' = ctx [("list", CList [CInt 1, CInt 2, CInt 3, CInt 5])]
-- >>> parseString ctx' getAllDefaultTags "{% for x in list %}\n{{ x }}{% endfor %}"
-- "\n1\n2\n3\n5"
parseString :: Context -> Tags -> String -> IO String
parseString ctx' tags' text =
    foldl1 (liftM2 (++)) $ parseTokens ctx' tags' $ tokenize text
