module Tpll.Tags.Default
(
    firstOfTag, nowTag,
    upperFilter, lowerFilter,
    getAllDefaultTags
) where


import Tpll.Context (Context, ctx, ContextValue(CStr, CInt, CList, CAssoc))
import Tpll.Tokenizer (Token(Tag, Variable, content, line))
import Tpll.Tags (TagAction(Render, RenderBlock), Tags, tags)


import Prelude hiding (lookup)
import Control.Monad (liftM2)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Map.Strict (lookup, insert)
import Data.List (elemIndex)
import Data.Char (toUpper, toLower)


-- | template tag: firstof
--
-- Examples:
--
-- >>> let ctx' = ctx [("a", CStr ""), ("b", CStr "x")]
-- >>> let Render (_, r) = firstOfTag ctx' (Tag { content = "firstof a b", line = 1 }) []
-- >>> r
-- "x"
--
-- >>> let Render (_, r) = firstOfTag ctx' (Tag { content = "firstof a c", line = 1 }) []
-- >>> r
-- ""
firstOfTag :: Context -> Token -> [Token] -> TagAction
firstOfTag ctx' token tokens =
    let Tag { content = content, line = _ } = token
        (_, parts) = splitAt 1 $ words content
    in
        Render (tokens, return $ firstOfTag' ctx' parts)

firstOfTag' :: Context -> [String] -> String
firstOfTag' _ [] = ""
firstOfTag' ctx' (x:xs) =
    case lookup x ctx' of
        Just (CStr x) ->
            if x == "" then
                firstOfTag' ctx' xs
            else
                x
        Just (CInt x) ->
            show x
        Nothing ->
            firstOfTag' ctx' xs


-- | template tag: now
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> let expected = fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%I:%S") getCurrentTime
-- >>> let Render (_, r) = nowTag ctx' (Tag { content = "now", line = 1 }) []
-- >>> :{
--  do
--      a <- expected
--      b <- r
--      print $ show $ a == b
-- :}
-- "True"
nowTag :: Context -> Token -> [Token] -> TagAction
nowTag ctx' token tokens =
    let result = fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%I:%S") getCurrentTime
    in
        Render ([], result)


-- | template tag: for
--
-- Examples:
--
-- >>> let ctx' = ctx [("list", CList [CInt 1, CInt 2, CInt 3, CInt 5])]
-- >>> let RenderBlock (ctxstack, oldctx, tokens, untilToken) = forTag ctx' (Tag { content = "for item in list", line = 1 }) [Variable { content = "item", line = 1 }, Tag { content = "endfor", line = 1 }]
-- >>> ctx' == oldctx
-- True
--
-- >>> tokens
-- [Variable {content = "item", line = 1},Tag {content = "endfor", line = 1}]
--
-- >>> untilToken
-- Tag {content = "endfor", line = 0}
--
-- >>> length ctxstack
-- 4
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
                    RenderBlock (ctxstack, ctx', tokens, (Tag { content = "endfor", line = 0 }))


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


-- | Upper template filter
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> let Just (CStr r) = upperFilter ctx' (Just (CStr "bar"))
-- >>> r
-- "BAR"
upperFilter :: Context -> Maybe ContextValue -> Maybe ContextValue
upperFilter ctx' val =
    case val of
        Just (CStr a) ->
            Just (CStr (map toUpper a))
        Just b ->
            Just b
        Nothing ->
            Nothing


-- | Lower template filter
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> let Just (CStr r) = lowerFilter ctx' (Just (CStr "FOO"))
-- >>> r
-- "foo"
lowerFilter :: Context -> Maybe ContextValue -> Maybe ContextValue
lowerFilter ctx' val =
    case val of
        Just (CStr a) ->
            Just (CStr (map toLower a))
        Just b ->
            Just b
        Nothing ->
            Nothing


getAllDefaultTags :: Tags
getAllDefaultTags =
    tags [
        ("firstof", firstOfTag),
        ("now", nowTag),
        ("for", forTag)
    ] [
        ("upper", upperFilter),
        ("lower", lowerFilter)
    ]
