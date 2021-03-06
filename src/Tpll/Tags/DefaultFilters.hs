{-|
Module      : Tpll.Tags.DefaultFilters
Description : A collection of default template filters
-}

module Tpll.Tags.DefaultFilters
(
    upperFilter, lowerFilter, capFirstFilter, titleFilter, firstFilter,
    safeFilter, defaultFilter, dateFilter, addFilter, lastFilter, joinFilter,
    ljustFilter, rjustFilter, truncatecharsFilter
)
where


import Tpll.Context (Context, ContextValue(CInt, CDouble, CStr, CList, CAssoc, CUTCTime, CIOUTCTime, CIOString), isSafe, ctxToString)
import Tpll.Tags.Utils (isFalse, formatUTCTime, formatIOUTCTime)


import Data.Char (toUpper, toLower)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad (liftM2)


-- | @{{ arg|upper }}@
--
-- Transforms text to uppercase.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cStr, cDouble)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("x", cStr "foo"), ("y", cDouble 3.14)]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|upper }}"
-- "FOO"
--
-- >>> parseString ctx' tags' "{{ y|upper }}"
-- "3.14"
upperFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
upperFilter _ val _ =
    case val of
        Just (CStr safe a) ->
            Just (CStr safe (map toUpper a))
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
-- >>> import Tpll.Context (ctx, cStr, cDouble)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("x", cStr "Bar"), ("y", cDouble 3.14)]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|lower }}"
-- "bar"
--
-- >>> parseString ctx' tags' "{{ y|lower }}"
-- "3.14"
lowerFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
lowerFilter _ val _ =
    case val of
        Just (CStr safe a) ->
            Just (CStr safe (map toLower a))
        Just b ->
            Just b
        Nothing ->
            Nothing


-- | @{{ arg|capfirst }}@
--
-- Capitalizes the first character of the value.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cStr)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("x", cStr "this is a string.")]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|capfirst }}"
-- "This is a string."
--
-- >>> parseString ctx' tags' "{{ y|capfirst }}"
-- ""
capFirstFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
capFirstFilter _ val _ =
    case val of
        Just (CStr safe (x:xs)) ->
            Just (CStr safe (toUpper x : xs))
        _ ->
            Nothing


-- | @{{ arg|title }}@
--
-- Capitalizes the first character of each words.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cStr)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("x", cStr "this is a string.")]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|title }}"
-- "This Is A String."
titleFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
titleFilter _ val _ =
    case val of
        Just (CStr safe str) ->
            let title = unwords $ map (\(x:xs) -> toUpper x : xs) $ words str
            in
                Just (CStr safe title)
        _ ->
            Nothing



-- | @{{ arg|first }}@
--
-- Returns the first item in a list.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cInt, cList)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("x", cList [cInt 42, cInt 1, cInt 2])]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|first }}"
-- "42"
firstFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
firstFilter _ val _ =
    case val of
        Just (CList _ (x:xs)) ->
            Just x
        _ ->
            Nothing


-- | @{{ arg|safe }}@
--
-- Marks a string as safe for priting HTML.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cStr)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("x", cStr "<b>foo</b>")]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x }}"
-- "&lt;b&gt;foo&lt;/b&gt;"
--
-- >>> parseString ctx' tags' "{{ x|safe }}"
-- "<b>foo</b>"
safeFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
safeFilter _ val _ =
    case val of
        Nothing ->
            Nothing
        Just v ->
            Just (v { isSafe = True })


-- | @{{ arg|default:"nothing" }}@
--
-- If value evaluates to @False@, uses the given value.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cStr)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("x", cStr "foo")]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|default:\"bar\" }}"
-- "foo"
--
-- >>> parseString ctx' tags' "{{ y|default:\"bar\" }}"
-- "bar"
defaultFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
defaultFilter _ val arg =
    if isFalse val then
        Just (CStr False "bar")
    else
        val


-- | @{{ arg|date:"format" }}@
--
-- Converts value to the given format.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cStr, cUTCTime, cIOUTCTime)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> import Data.Time.Calendar (fromGregorian)
-- >>> import Data.Time.Clock
-- >>>
-- >>> let ioutctime = return $ UTCTime (fromGregorian 2011 12 16) (fromIntegral $ 12 * 3600)
-- >>> let utctime = UTCTime (fromGregorian 2011 12 16) (fromIntegral $ 12 * 3600)
--
-- >>> let ctx' = ctx [("x", cIOUTCTime ioutctime), ("y", cUTCTime utctime)]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|date }}"
-- "2011-12-16 12:12:00"
--
-- >>> parseString ctx' tags' "{{ x|date:\"%Y-%m-%d\" }}"
-- "2011-12-16"
--
-- >>> parseString ctx' tags' "{{ y|date }}"
-- "2011-12-16 12:12:00"
--
-- >>> parseString ctx' tags' "{{ y|date:\"%Y-%m-%d\" }}"
-- "2011-12-16"
dateFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
dateFilter _ val format =
    case val of
        Just (CIOUTCTime safe utctime) ->
            let result = formatIOUTCTime utctime format
            in
                Just (CIOString safe result)
        Just (CUTCTime safe utctime) ->
            let result = formatUTCTime utctime format
            in
                Just (CStr safe result)
        Nothing ->
            Nothing


-- | @{{ arg|add:1 }}@
--
-- Adds the argument to the value
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cInt, cDouble, cStr)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
--
-- >>> let ctx' = ctx [("x", cInt 1), ("y", cStr "foo"), ("z", cDouble 1.0)]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|add:1 }}"
-- "2"
--
-- >>> parseString ctx' tags' "{{ y|add:\"bar\" }}"
-- "foobar"
--
-- >>> parseString ctx' tags' "{{ z|add:2.14 }}"
-- "3.14"
--
-- >>> parseString ctx' tags' "{{ z|add:2 }}"
-- "3.0"
--
-- >>> parseString ctx' tags' "{{ y|add:2 }}"
-- "foo"
addFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
addFilter _ val arg =
    case arg of
        Just (CInt _ x) ->
            case val of
                Just (CInt safe y) ->
                    Just (CInt safe (x + y))
                Just (CDouble safe y) ->
                    Just (CDouble safe (fromIntegral x + y))
                _ ->
                    val
        Just (CDouble _ x) ->
            case val of
                Just (CDouble safe y) ->
                    Just (CDouble safe (x + y))
                Just (CInt safe y) ->
                    Just (CDouble safe (x + fromIntegral y))
                _ ->
                    val
        Just (CStr _ x) ->
            case val of
                Just (CStr safe y) ->
                    Just (CStr safe (y ++ x))
                _ ->
                    val
        _ ->
            Nothing


-- | @{{ arg|last }}@
--
-- Returns the last item in a list.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cInt, cDouble, cStr, cList, cAssoc)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
--
-- >>> let ctx' = ctx [("x", cList [cInt 1, cInt 2]), ("y", cStr "foo"), ("z", cAssoc (cInt 3, cInt 4))]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|last }}"
-- "2"
--
-- >>> parseString ctx' tags' "{{ y|last }}"
-- "o"
--
-- >>> parseString ctx' tags' "{{ z|last }}"
-- "4"
--
-- >>> parseString ctx' tags' "{{ unknown|last }}"
-- ""
lastFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
lastFilter _ Nothing _ =
    Nothing
lastFilter _ (Just (CList _ lst)) _ =
    Just (last lst)
lastFilter _ (Just (CStr safe str)) _ =
    Just (CStr safe [last str])
lastFilter _ (Just (CAssoc _ (_, y))) _ =
    Just y


-- | @{{ arg|join:"separator"" }}@
--
-- Joins a list of strings.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cInt, cDouble, cStr, cList, cAssoc)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
--
-- >>> let ctx' = ctx [("foo", cList [cStr "foo", cStr "bar"])]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ foo|join }}"
-- "foobar"
--
-- >>> parseString ctx' tags' "{{ foo|join:\" -- \" }}"
-- "foo -- bar"
joinFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
joinFilter _ val (Just (CStr _ sep)) =
    case val of
        Just (CList safe lst) ->
            let strings = map (ctxToString . Just) lst
                result = foldl1 (liftM2 (\a b -> a ++ sep ++ b)) strings
            in
                Just (CIOString safe result)
        _ ->
            Nothing
joinFilter ctx' val _ =
    joinFilter ctx' val (Just (CStr False ""))


-- | @{{ arg|ljust:10 }}@
--
-- Left-aligns the value in a field of a given width.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cStr, cInt)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
--
-- >>> let ctx' = ctx [("a", cStr "foo"), ("b", cStr "haskell"), ("n", cInt 7)]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ a|ljust:10 }}"
-- "foo       "
--
-- >>> parseString ctx' tags' "{{ b|ljust:n }}"
-- "haskell"
ljustFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
ljustFilter _ a@(Just (CStr safe str)) (Just (CInt _ n)) =
    let strl = length str
    in
        if n > strl then
            Just (CStr safe (str ++ (concat $ replicate (n - strl) " ")))
        else
            a
ljustFilter _ _ _ =
    Nothing


-- | @{{ arg|rjust:10 }}@
--
-- Right-aligns the value in a field of a given width.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cStr, cInt)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
--
-- >>> let ctx' = ctx [("a", cStr "foo"), ("b", cStr "haskell"), ("n", cInt 7)]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ a|rjust:10 }}"
-- "       foo"
--
-- >>> parseString ctx' tags' "{{ b|rjust:n }}"
-- "haskell"
rjustFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
rjustFilter _ a@(Just (CStr safe str)) (Just (CInt _ n)) =
    let strl = length str
    in
        if n > strl then
            Just (CStr safe ((concat $ replicate (n - strl) " ") ++ str))
        else
            a
rjustFilter _ _ _ =
    Nothing


-- | @{{ arg|truncatechars:10 }}@
--
-- Truncates a string if it is longer than the specified number of characters.
-- Truncated strings will end with a translatable ellipsis sequence (”...”).
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, cStr, cInt)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
--
-- >>> let ctx' = ctx [("a", cStr "foo"), ("b", cStr "haskell"), ("n", cInt 3)]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ a|truncatechars:3 }}"
-- "foo"
--
-- >>> parseString ctx' tags' "{{ b|truncatechars:n }}"
-- "has..."
truncatecharsFilter :: Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
truncatecharsFilter _ a@(Just (CStr safe str)) (Just (CInt _ n)) =
    let strl = length str
    in
        if n < strl then
            let (x, _) = splitAt n str
            in
                Just (CStr safe (x ++ "..."))
        else
            a
truncatecharsFilter _ _ _ =
    Nothing
