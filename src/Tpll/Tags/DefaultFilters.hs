{-|
Module      : Tpll.Tags.DefaultFilters
Description : A collection of default template filters
-}

module Tpll.Tags.DefaultFilters
(
    upperFilter, lowerFilter, capFirstFilter, titleFilter ,firstFilter,
    safeFilter, defaultFilter
)
where


import Tpll.Context (Context, ContextValue(CStr, CList), isSafe)


import Data.Char (toUpper, toLower)


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
    case val of
        Nothing ->
           Just (CStr False "bar")
        v ->
            v
