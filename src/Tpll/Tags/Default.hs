{-|
Module      : Tpll.Tags.Default
-}

module Tpll.Tags.Default
(
    commentTag, firstOfTag, nowTag,
    upperFilter, lowerFilter,
    getAllDefaultTags
) where


import Tpll.Context (Context, ctx, ContextValue(CStr, CInt, CDouble, CList, CAssoc), resolveCtx, ctxToString)
import Tpll.Tokenizer (Token(Tag, Variable, Text, Comment, content, line, raw))
import Tpll.Tags (TagAction(Render, RenderBlock), Tags, tags)
import Tpll.Tags.Utils (resolveParts)


import Prelude hiding (lookup)
import Control.Monad (liftM2)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Map.Strict (lookup, insert)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Char (toUpper, toLower)


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
commentTag :: Context -> Token -> [Token] -> TagAction
commentTag ctx' _ tokens =
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
-- >>> let ctx' = ctx [("a", CStr ""), ("b", CStr "x")]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{% firstof a b %}"
-- "x"
--
-- >>> parseString ctx' tags' "{% firstof a c %}"
-- ""
firstOfTag :: Context -> Token -> [Token] -> TagAction
firstOfTag ctx' token tokens =
    let Tag { content = content, line = _ } = token
        (_, parts) = splitAt 1 $ words content
    in
        Render (tokens, return $ firstOfTag' ctx' $ resolveParts ctx' parts)


firstOfTag' :: Context -> [Maybe ContextValue] -> String
firstOfTag' _ [] = ""
firstOfTag' ctx' (x:xs) =
    case x of
        Just (CStr x) ->
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
-- >>> let ctx' = ctx [("a", CStr ""), ("b", CStr "x")]
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
nowTag :: Context -> Token -> [Token] -> TagAction
nowTag ctx' token tokens =
    let Tag { content = content, line = _ } = token
        parts = tail $ words content

        format' = case parts of
            [] -> Nothing
            (x:_) -> resolveCtx ctx' x

        format = case format' of
            Just (CStr x) -> x
            _ -> "%Y-%m-%d %H:%I:%S"

    in
        let result = fmap (formatTime defaultTimeLocale format) getCurrentTime
        in
            Render ([], result)


-- | @{% for x in list %} ... {% endfor %}@
--
-- Iterates over all items in the list.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("list", CList [CInt 1, CInt 2, CInt 3, CInt 5])]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{% for x in list %}{{ x }},{% endfor %}"
-- "1,2,3,5,"
--
forTag :: Context -> Token -> [Token] -> TagAction
forTag ctx' token tokens =
    let Tag { content = content, line = _ } = token
        (_, parts) = splitAt 1 $ words content
    in
        case "in" `elemIndex` parts of
            Nothing ->
                Render ([], return "<error>")
            Just pos ->
                let ([vars], [_, arr]) = splitAt pos parts
                    ctxstack = case lookup arr ctx' of
                        Just lst ->
                            forTagStack ctx' vars lst
                        Nothing ->
                            []
                in
                    RenderBlock (ctxstack, ctx', tokens, "endfor")


-- | Create a context stack for the "for" loop
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", CStr "bar")]
-- >>> let lst = CList [CInt 1, CInt 2, CInt 3]
-- >>> let ctxstack = forTagStack ctx' "i" lst
-- >>> length ctxstack
-- 3
forTagStack :: Context -> String -> ContextValue -> [Context]
forTagStack ctx' key val =
    case val of
        (CList lst) ->
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
withTag :: Context -> Token -> [Token] -> TagAction
withTag ctx' token tokens =
    let Tag { content = content, line = _ } = token
        parts = tail $ words content
        kvs = map (splitOn "=") $ parts
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
verbatimTag :: Context -> Token -> [Token] -> TagAction
verbatimTag ctx' _ tokens =
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
    


-- | @{{ arg|upper }}@
--
-- Transforms text to uppercase.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("x", CStr "foo"), ("y", CDouble 3.14)]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|upper }}"
-- "FOO"
--
-- >>> parseString ctx' tags' "{{ y|upper }}"
-- "3.14"
upperFilter :: Context -> Maybe ContextValue -> Maybe ContextValue
upperFilter ctx' val =
    case val of
        Just (CStr a) ->
            Just (CStr (map toUpper a))
        Just b ->
            Just b
        Nothing ->
            Nothing


-- | @{{ arg|lower }}@
--
-- Transforms text to lowercase.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("x", CStr "Bar"), ("y", CDouble 3.14)]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|lower }}"
-- "bar"
--
-- >>> parseString ctx' tags' "{{ y|lower }}"
-- "3.14"
lowerFilter :: Context -> Maybe ContextValue -> Maybe ContextValue
lowerFilter ctx' val =
    case val of
        Just (CStr a) ->
            Just (CStr (map toLower a))
        Just b ->
            Just b
        Nothing ->
            Nothing


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
        ("lower", lowerFilter)
    ]
