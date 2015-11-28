{-|
Module      : Tpll.Parser
-}

module Tpll.Parser
(
    parseString,
    parseTokens
) where


import Tpll.Tokenizer (Token(Tag, Variable, Text, content, line, raw), tokenize)
import Tpll.Context (Context, ctx, ContextValue(CStr, CInt, CList), resolveCtx, ctxToString)
import Tpll.Tags (Tag, Tags, TagAction(Render, RenderBlock), tags, Filter)
import Tpll.Tags.Default (getAllDefaultTags, upperFilter, lowerFilter)
import Text.Regex.PCRE ((=~))


import Prelude hiding (lookup)
import Control.Monad (liftM2)
import Data.Map.Strict (lookup)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)


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
consumeTag ctx' (tags', _) token tokens =
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
-- >>> let tags' = tags [] []
-- >>> let Just (CStr r) = lookup "foo" ctx'
-- >>> r
-- "bar"
-- >>> let [r] = parseTokens ctx' tags' [Variable { content = "foo", line = 1, raw = "{{ foo }}" }]
-- >>> r
-- "bar"
-- >>> let [r] = parseTokens ctx' tags' [Variable { content = "unknown", line = 1, raw = "{{ unknown }}" }]
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


renderBlock :: [Context] -> Tags -> [Token] -> String -> [IO String] -> ([Token], IO String)
renderBlock [] _ _ _ acc =
    ([], foldl1 (liftM2 (++)) $ reverse acc)
renderBlock (ctx':ctxstack) tags' tokens untilToken acc =
    let result = parseTokensUntil ctx' tags' tokens untilToken
    in
        renderBlock ctxstack tags' tokens untilToken (result ++ acc)


-- | Parse tokens until tag token
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", CStr "bar"), ("bar", CStr "2")]
-- >>> let tags' = tags [] []
-- >>> let tokens = [Variable { content = "foo", line = 1, raw = "{{ foo }}" }, Tag { content = "x", line = 1, raw = "{% x %}" }, Variable { content = "bar", line = 2, raw = "{{ bar }}" }]
-- >>> let r = parseTokensUntil ctx' tags' tokens "x|bar"
-- >>> length r
-- 1
-- >>> head r
-- "bar"
parseTokensUntil :: Context -> Tags -> [Token] -> String -> [IO String]
parseTokensUntil ctx' tags' tokens untilToken =
    parseTokensUntil' ctx' tags' tokens untilToken []


parseTokensUntil' :: Context -> Tags -> [Token] -> String -> [IO String] -> [IO String]
parseTokensUntil' _ _ [] untilToken acc =
    reverse acc

parseTokensUntil' ctx' tags' (token:tokens) untilToken acc =
    case token of
        Tag { content = c } ->
            let currentToken = head $ words c
            in
                if currentToken =~ untilToken then
                    acc
                else
                    let (newtokens, result) = parseToken ctx' tags' (token:tokens)
                    in
                        parseTokensUntil' ctx' tags' newtokens untilToken (result:acc)
        _ ->
            let (newtokens, result) = parseToken ctx' tags' (token:tokens)
            in
                parseTokensUntil' ctx' tags' newtokens untilToken (result:acc)


-- | Parse token
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", CStr "foo")]
-- >>> let tags' = tags [] []
-- >>> let (_, r) = parseToken ctx' tags' [Variable { content = "foo", line = 1, raw = "{{ foo }}" }]
-- >>> r
-- "foo"
parseToken :: Context -> Tags -> [Token] -> ([Token], IO String)
parseToken ctx' tags' (token:tokens) =
    case token of
        Variable { content = _, line = _ } ->
            (tokens, return $ parseTokenVariable ctx' tags' token)
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
-- >>> let ctx' = ctx [("foo", CStr "foo")]
-- >>> let tags' = tags [] []
-- >>> parseTokenVariable ctx' tags' (Variable { content = "foo", line = 0, raw = "{{ foo }}" })
-- "foo"
--
-- >>> let ctx' = ctx [("foo", CStr "foo")]
-- >>> let tags' = getAllDefaultTags
-- >>> parseTokenVariable ctx' tags' (Variable { content = "foo|upper", line = 0, raw = "{{ foo|upper }}" })
-- "FOO"
--
-- >>> let ctx' = ctx [("foo", CStr "foo")]
-- >>> let tags' = getAllDefaultTags
-- >>> parseTokenVariable ctx' tags' (Variable { content = "\"x Y z\"|upper|lower", line = 0, raw = "{{ \"x Y z\"|upper|lower }}" })
-- "x y z"
parseTokenVariable :: Context -> Tags -> Token -> String
parseTokenVariable ctx' (_, filters') (Variable { content = content, line = _ }) =
    let (key:filters) = splitOn "|" content
        val = resolveCtx ctx' key
        pipeline = mapMaybe (`lookup` filters') filters
        result = runFilters ctx' val pipeline
    in
        ctxToString result


-- | Run filter pipeline over value
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> let filters = [upperFilter]
-- >>> let Just (CStr r) = runFilters ctx' (Just (CStr "foo")) filters
-- >>> r
-- "FOO"
--
-- >>> let ctx' = ctx []
-- >>> let filters = []
-- >>> let Just (CStr r) = runFilters ctx' (Just (CStr "foo")) filters
-- >>> r
-- "foo"
--
-- >>> let ctx' = ctx []
-- >>> let filters = [upperFilter, lowerFilter]
-- >>> let Just (CStr r) = runFilters ctx' (Just (CStr "fooBar")) filters
-- >>> r
-- "foobar"
runFilters :: Context -> Maybe ContextValue -> [Filter] -> Maybe ContextValue
runFilters ctx' val [] = val
runFilters ctx' val (fn:filters) =
    runFilters ctx' (fn ctx' val) filters


parseTokenTag :: Context -> Tags -> Token -> [Token] -> ([Token], IO String)
parseTokenTag ctx' tags' token tokens =
    let result = consumeTag ctx' tags' token tokens
    in
        case result of
            RenderBlock (ctxstack, oldctx, newtokens, untilToken) ->
                renderBlock ctxstack tags' newtokens untilToken []
            Render (newtokens, render) ->
                (newtokens, render)


-- | Parse string
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> parseString ctx' getAllDefaultTags "foo{% firstof x \"bar\" %}bar"
-- "foobarbar"
--
-- >>> let ctx' = ctx []
-- >>> parseString ctx' getAllDefaultTags "foo{% firstof 1 2 %}bar"
-- "foo1bar"
--
-- >>> let ctx' = ctx []
-- >>> parseString ctx' getAllDefaultTags "foo{% firstof 2.0 %}bar"
-- "foo2.0bar"
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
--
-- >>> let ctx' = ctx [("list", CList [CStr "a", CStr "b"])]
-- >>> parseString ctx' getAllDefaultTags "{% for x in list %}\n{{ x|upper }}{% endfor %}"
-- "\nA\nB"
parseString :: Context -> Tags -> String -> IO String
parseString ctx' tags' text =
    foldl1 (liftM2 (++)) $ parseTokens ctx' tags' $ tokenize text
