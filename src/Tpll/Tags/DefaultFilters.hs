{-|
Module      : Tpll.Tags.DefaultFilters
Description : A collection of default template filters
-}

module Tpll.Tags.DefaultFilters
(
    upperFilter, lowerFilter, capFirstFilter
)
where


import Tpll.Context (Context, ContextValue(CStr))


import Data.Char (toUpper, toLower)


-- | @{{ arg|upper }}@
--
-- Transforms text to uppercase.
--
-- __Examples:__
--
-- >>> import Tpll.Parser (parseString)
-- >>> import Tpll.Context (ctx, ContextValue(CStr, CDouble))
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
upperFilter _ val =
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
-- >>> import Tpll.Context (ctx, ContextValue(CStr, CDouble))
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
lowerFilter _ val =
    case val of
        Just (CStr a) ->
            Just (CStr (map toLower a))
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
-- >>> import Tpll.Context (ctx, ContextValue(CStr))
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>>
-- >>> let ctx' = ctx [("x", CStr "this is a string.")]
-- >>> let tags' = getAllDefaultTags
--
-- >>> parseString ctx' tags' "{{ x|capfirst }}"
-- "This is a string."
--
-- >>> parseString ctx' tags' "{{ y|capfirst }}"
-- ""
capFirstFilter :: Context -> Maybe ContextValue -> Maybe ContextValue
capFirstFilter _ val =
    case val of
        Just (CStr (x:xs)) ->
            Just (CStr (toUpper x : xs))
        _ ->
            Nothing
