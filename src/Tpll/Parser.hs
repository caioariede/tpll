{-|
Module      : Tpll.Parser
Description : Functions to parse tokens
-}

module Tpll.Parser
(
    parseString,
    parseTokens
) where


import Tpll.Tokenizer (Token(Tag, Variable, Text, content, line, raw), tokenize)
import Tpll.Context (Context, ctx, ContextValue(CStr, CInt, CList), cStr, cInt, cList, resolveCtx, ctxToString)
import Tpll.Tags (Tag, Tags(Tags), TagAction(Render, RenderBlock), tags, Filter)
import Tpll.Tags.Default (getAllDefaultTags)
import Tpll.Tags.Utils (resolveValue)
import Text.Regex.PCRE ((=~))


import Prelude hiding (lookup)
import Control.Monad (liftM2)
import Data.Map.Strict (lookup)


-- | Consume tag token
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> let tags' = tags [] []
-- >>> let Render (_, r) = consumeTag ctx' tags' (Tag { content = "x", line = 1, raw = "{% x %}" }) []
-- >>> r
-- "<error>"
consumeTag :: Context -> Tags -> Token -> [Token] -> TagAction
consumeTag ctx' tags' token tokens =
    let Tag { content = content, line = _ } = token
        Tags (onlytags, _) = tags'
        ([name], parts) = splitAt 1 $ words content
    in
        case lookup name onlytags of
            Just fn ->
                fn ctx' tags' token tokens
            Nothing ->
                Render (tokens, return "<error>")


-- | Parse tokens
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", cStr "bar"), ("bar", cStr "2")]
-- >>> let tags' = tags [] []
-- >>> let Just (CStr False r) = lookup "foo" ctx'
-- >>> r
-- "bar"
-- >>> let [a, b] = parseTokens ctx' tags' [Variable { content = "foo", line = 1, raw = "{{ foo }}" }, Text { content = "foo", line = 1, raw = "" }]
-- >>> a
-- "bar"
-- >>> b
-- "foo"
-- >>> let [a] = parseTokens ctx' tags' [Variable { content = "unknown", line = 1, raw = "{{ unknown }}" }]
-- >>> a
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


-- | Render a block
--
-- Examples:
--
-- >>> let ctx1 = ctx [("foo", cStr "abc")]
-- >>> let ctx2 = ctx [("foo", cStr "def")]
-- >>>
-- >>> let tags' = tags [] []
-- >>> --- let tokens = [Variable { content = "foo", line = 1, raw = "{{ foo }}" }]
-- >>> let tokens = [Text { content = "\n", line = 1, raw = "" }, Variable { content = "foo", line = 1, raw = "{{ foo }}" }]
-- >>>
-- >>> renderBlock [ctx1, ctx2] tags' tokens []
-- "\nabc\ndef"
renderBlock :: [Context] -> Tags -> [Token] -> [IO String] -> IO String
renderBlock [] _ _ acc =
    foldl1 (liftM2 (++)) acc
renderBlock (ctx':ctxstack) tags' tokens acc =
    let result = parseTokens ctx' tags' tokens
    in
        renderBlock ctxstack tags' tokens (acc ++ result)


-- | Parse token
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", cStr "foo")]
-- >>> let tags' = tags [] []
-- >>> let (_, r) = parseToken ctx' tags' [Variable { content = "foo", line = 1, raw = "{{ foo }}" }]
-- >>> r
-- "foo"
parseToken :: Context -> Tags -> [Token] -> ([Token], IO String)
parseToken ctx' tags' (token:tokens) =
    case token of
        Variable { content = _, line = _ } ->
            (tokens, parseTokenVariable ctx' tags' token)
        Text { content = content, line = _ } ->
            (tokens, return content)
        Tag { content = _, line = _ } ->
            parseTokenTag ctx' tags' token tokens
        _ ->
            (tokens, return "")


-- | Parse variable token
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", cStr "foo")]
-- >>> let tags' = tags [] []
-- >>> parseTokenVariable ctx' tags' (Variable { content = "foo", line = 0, raw = "{{ foo }}" })
-- "foo"
--
-- >>> let ctx' = ctx [("foo", cStr "foo")]
-- >>> let tags' = getAllDefaultTags
-- >>> parseTokenVariable ctx' tags' (Variable { content = "foo|upper", line = 0, raw = "{{ foo|upper }}" })
-- "FOO"
--
-- >>> let ctx' = ctx [("foo", cStr "foo")]
-- >>> let tags' = getAllDefaultTags
-- >>> parseTokenVariable ctx' tags' (Variable { content = "\"x Y z\"|upper|lower", line = 0, raw = "{{ \"x Y z\"|upper|lower }}" })
-- "x y z"
parseTokenVariable :: Context -> Tags -> Token -> IO String
parseTokenVariable ctx' tags' (Variable { content = content, line = _ }) =
    ctxToString $ resolveValue ctx' tags' content


parseTokenTag :: Context -> Tags -> Token -> [Token] -> ([Token], IO String)
parseTokenTag ctx' tags' token tokens =
    let result = consumeTag ctx' tags' token tokens
    in
        case result of
            RenderBlock (ctxstack, oldctx, tokens, rest) ->
                let render = renderBlock ctxstack tags' tokens []
                in
                    (rest, render)
            Render (rest, render) ->
                (rest, render)


-- | Parse string
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> parseString ctx' getAllDefaultTags "foo{% firstof x \"bar\"|upper %}bar"
-- "fooBARbar"
--
-- >>> let ctx' = ctx []
-- >>> parseString ctx' getAllDefaultTags "foo{% firstof 1 2 %}bar"
-- "foo1bar"
--
-- >>> let ctx' = ctx []
-- >>> parseString ctx' getAllDefaultTags "foo{% firstof 2.0 %}bar"
-- "foo2.0bar"
--
-- >>> let ctx' = ctx [("bang", cStr ""), ("bar", cInt 2)]
-- >>> parseString ctx' getAllDefaultTags "foo{% firstof bang %}bar"
-- "foobar"
--
-- >>> parseString ctx' getAllDefaultTags "foo{% firstof bang bar %}bar"
-- "foo2bar"
--
-- >>> let ctx' = ctx [("list", cList [cInt 1, cInt 2, cInt 3, cInt 5])]
-- >>> parseString ctx' getAllDefaultTags "{% for x in list %}\n{{ x }}{% endfor %}"
-- "\n1\n2\n3\n5"
--
-- >>> let ctx' = ctx [("list", cList [cStr "a", cStr "b"])]
-- >>> parseString ctx' getAllDefaultTags "{% for x in list %}\n{{ x|upper }}{% endfor %}"
-- "\nA\nB"
parseString :: Context -> Tags -> String -> IO String
parseString ctx' tags' text =
    foldl1 (liftM2 (++)) $ parseTokens ctx' tags' $ tokenize text
