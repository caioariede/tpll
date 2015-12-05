{-|
Module      : Tpll.Tags.Default
Description : A collection of default template tags
-}

module Tpll.Tags.Default
(
    commentTag, firstOfTag, nowTag, forTag,
    getAllDefaultTags
) where


import Tpll.Context (Context, ctx, ContextValue(CStr, CInt, CDouble, CList, CAssoc), cStr, cList, cInt, resolveCtx, ctxToString)
import Tpll.Tokenizer (Token(Tag, Variable, Text, Comment, content, line, raw))
import Tpll.Tags (TagAction(Render, RenderBlock), Tags, tags)
import Tpll.Tags.Utils (resolveParts)
import Tpll.Tags.DefaultFilters (lowerFilter, upperFilter, capFirstFilter,
    firstFilter, safeFilter)


import Prelude hiding (lookup)
import Control.Monad (liftM2)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Map.Strict (lookup, insert)
import Data.List (elemIndex)
import Data.List.Split (splitOn)


-- | @{% comment %} ... {% endcomment %}@
--
-- Ignores everything between @{% comment %}@ and @{% endcomment %}@
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx []
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "abra{% comment %} X Y {% endcomment %}cadabra"
-- "abracadabra"
commentTag :: Context -> Tags -> Token -> [Token] -> TagAction
commentTag ctx' _ _ tokens =
    Render (commentTag' ctx' tokens, return "")

commentTag' :: Context -> [Token] -> [Token]
commentTag' _ [] = []
commentTag' ctx' (token:tokens) =
    case token of
        Tag { content = c, line = _ } ->
            let name = head $ words c
            in
                if name == "endcomment" then
                    tokens
                else
                    commentTag' ctx' tokens
        _ ->
            commentTag' ctx' tokens


-- | @{% firstof arg1 arg2 arg3 ... %}@
--
-- Returns the first argument that is not empty.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("a", cStr ""), ("b", cStr "x")]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{% firstof a b %}"
-- "x"
--
-- >>> parseString ctx' tags' "{% firstof a c %}"
-- ""
firstOfTag :: Context -> Tags -> Token -> [Token] -> TagAction
firstOfTag ctx' tags' token tokens =
    let Tag { content = content, line = _ } = token
        (_, parts) = splitAt 1 $ words content
    in
        Render (tokens, return $ firstOfTag' ctx' $ resolveParts ctx' tags' parts)


firstOfTag' :: Context -> [Maybe ContextValue] -> String
firstOfTag' _ [] = ""
firstOfTag' ctx' (x:xs) =
    case x of
        Just (CStr _ x) ->
            if x == "" then
                firstOfTag' ctx' xs
            else
                x
        Nothing ->
            firstOfTag' ctx' xs
        _ ->
            ctxToString x


-- | @{% now [optional format] %}@
--
-- Returns the current date and time. Accepts one argument with the desired
-- format. The format must use the same syntax and codes defined by the
-- `Data.Time.Format.formatTime` function.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("a", cStr ""), ("b", cStr "x")]
-- >>> let tags' = getAllDefaultTags
--
-- >>> import Data.Time.Clock (getCurrentTime)
-- >>> import Data.Time.Format (formatTime, defaultTimeLocale)
--
-- >>> -- Expected format: 2015-11-26 23:11:25
-- >>> let expected = fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%I:%S") getCurrentTime
--
-- >>> let result = parseString ctx' tags' "{% now %}"  -- output: 2015-11-26 23:11:25
-- >>> :{
--  do
--      a <- expected
--      b <- result
--      print $ id $ a == b
-- :}
-- True
--
-- >>> -- Expected format: 2015-11-26
-- >>> let expected = fmap (formatTime defaultTimeLocale "%Y-%m-%d") getCurrentTime
--
-- >>> let result = parseString ctx' tags' "{% now \"%Y-%m-%d\" %}"  -- output: 2015-11-26
-- >>> :{
--  do
--      a <- expected
--      b <- result
--      print $ id $ a == b
-- :}
-- True
nowTag :: Context -> Tags -> Token -> [Token] -> TagAction
nowTag ctx' _ token tokens =
    let Tag { content = content, line = _ } = token
        parts = tail $ words content

        format' = case parts of
            [] -> Nothing
            (x:_) -> resolveCtx ctx' x

        format = case format' of
            Just (CStr _ x) -> x
            _ -> "%Y-%m-%d %H:%I:%S"

    in
        let result = fmap (formatTime defaultTimeLocale format) getCurrentTime
        in
            Render ([], result)


-- | @{% for x in list %} ... {% endfor %}@
--
-- @{% for x in list %} ... {% empty %} fallback {% endfor %}@
--
-- Iterates over all items in the list. If the @list@ is empty, render the @empty@ block if present.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("list", cList [cInt 1, cInt 2, cInt 3, cInt 5])]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{% for x in list %}{{ x }},{% endfor %}"
-- "1,2,3,5,"
--
-- >>> parseString ctx' tags' "{% for x in unknown %}{{ x }},{% empty %}foo{% endfor %}"
-- "foo"
--
-- >>> parseString ctx' tags' "{% for x in list %}{{ x }}{% empty %}foo{% endfor %}"
-- "1235"
--
forTag :: Context -> Tags -> Token -> [Token] -> TagAction
forTag ctx' _ token tokens =
    let Tag { content = content, line = _ } = token
        (_, parts) = splitAt 1 $ words content
    in
        case "in" `elemIndex` parts of
            Nothing ->
                Render ([], return "<error>")
            Just pos ->
                let ([vars], [_, arr]) = splitAt pos parts
                in
                    case lookup arr ctx' of
                        Just lst ->
                            let ctxstack = forTagStack ctx' vars lst
                            in
                                RenderBlock (ctxstack, ctx', tokens, "endfor|empty")
                        Nothing ->
                            forTagEmpty ctx' tokens


forTagEmpty :: Context -> [Token] -> TagAction
forTagEmpty ctx' (token:tokens) =
    case token of
        Tag { content = content } ->
            let (tag:_) = words content
            in
                if tag == "empty" then
                    RenderBlock ([ctx'], ctx', tokens, "endfor")
                else
                    forTagEmpty ctx' tokens
        _ ->
            forTagEmpty ctx' tokens


-- | Create a context stack for the "for" loop
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", cStr "bar")]
-- >>> let lst = cList [cInt 1, cInt 2, cInt 3]
-- >>> let ctxstack = forTagStack ctx' "i" lst
-- >>> length ctxstack
-- 3
forTagStack :: Context -> String -> ContextValue -> [Context]
forTagStack ctx' key val =
    case val of
        (CList _ lst) ->
            map (\(a) -> insert key a ctx') lst
        _ ->
            []


-- | @{{% with arg1=value arg2=value %} ... {% endwith %}@
--
-- Assign values to variables.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx []
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{% with x=\"foo\" %}{{ x }}{% endwith %}"
-- "foo"
--
-- >>> parseString ctx' tags' "{% with x=\"foo\" y=2 %}{{ y }}{% endwith %}"
-- "2"
withTag :: Context -> Tags -> Token -> [Token] -> TagAction
withTag ctx' _ token tokens =
    let Tag { content = content, line = _ } = token
        parts = tail $ words content
        kvs = map (splitOn "=") parts
        newCtx = withTagCtx ctx' kvs
    in
        RenderBlock ([newCtx], ctx', tokens, "endwith")

withTagCtx :: Context -> [[String]] -> Context
withTagCtx ctx' [] = ctx'
withTagCtx ctx' ([key, value]:xs) =
    let lookup = resolveCtx ctx' value
    in
        case lookup of
            Nothing ->
                withTagCtx ctx' xs
            Just a ->
                withTagCtx (insert key a ctx') xs


-- | @{% verbatim %} ... {% endverbatim %}@
--
-- Avoids template rendering.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx []
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{% verbatim %}{{ x }}{%  y %}{# z  #} {% endverbatim %}"
-- "{{ x }}{%  y %}{# z  #} "
verbatimTag :: Context -> Tags -> Token -> [Token] -> TagAction
verbatimTag ctx' _ _ tokens =
    verbatimTagRender tokens []

verbatimTagRender :: [Token] -> String -> TagAction
verbatimTagRender [] str =
    Render ([], return str)
verbatimTagRender (token:tokens) str =
    case token of
        Tag { content = c, raw = r } ->
            let name = head $ words c
            in
                if name == "endverbatim" then
                    Render (tokens, return str)
                else
                    verbatimTagRender tokens (str ++ r)
        Text { content = c } ->
            verbatimTagRender tokens (str ++ c)
        Variable { raw = r } ->
            verbatimTagRender tokens (str ++ r)
        Comment { raw = r } ->
            verbatimTagRender tokens (str ++ r)


-- | Returns all default template tags and filters.
getAllDefaultTags :: Tags
getAllDefaultTags =
    tags [
        ("comment", commentTag),
        ("firstof", firstOfTag),
        ("now", nowTag),
        ("for", forTag),
        ("with", withTag),
        ("verbatim", verbatimTag)
    ] [
        ("upper", upperFilter),
        ("lower", lowerFilter),
        ("capfirst", capFirstFilter),
        ("first", firstFilter),
        ("safe", safeFilter)
    ]
