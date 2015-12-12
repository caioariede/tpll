{-|
Module      : Tpll.Tags.Utils
Description : Utility functions often used by template tags and filters
-}

module Tpll.Tags.Utils
(
    resolveParts,
    resolveValue,
    isFalse
)
where


import Tpll.Context (
    ContextValue(CStr, CInt, CInteger, CDouble, CList),
    cStr, cInt, cInteger, cDouble, cList,
    Context, ctx, resolveCtx, ctxToString)

import Tpll.Tags (Tags(Tags), Filter, FilterMap, tags)


import Prelude hiding (lookup)
import Data.Map.Strict (lookup)
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)


-- | Resolve parts of a template tag
--
-- Examples:
--
-- >>> let ctx' = ctx [("a", cInt 1)]
-- >>> let tags' = tags [] []
-- >>> let [Just (CInt False 1), Nothing] = resolveParts ctx' tags' ["a", "b"]
resolveParts :: Context -> Tags -> [String] -> [Maybe ContextValue]
resolveParts ctx' tags' =
    map (resolveValue ctx' tags')


-- | Resolve a string argument to a ContextValue
--
-- A string argument have one of these formats:
--
-- @"foo"@ which represents a string
--
-- @foo@ which represents a variable
--
-- @foo|filter1|filter2@ which represents a value that will be pipelined through these filters
--
-- Examples:
--
-- >>> import Tpll.Context (ctx)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>> import Tpll.Tags.DefaultFilters (upperFilter)
--
-- >>> let ctx' = ctx []
-- >>> let tags' = getAllDefaultTags

-- >>> resolveValue ctx' tags' "\"foo\""
-- "foo"
resolveValue :: Context -> Tags -> String -> Maybe ContextValue
resolveValue ctx' (Tags (_, filters')) value =
    let (key:filters) = splitOn "|" value
        val = resolveCtx ctx' key
        filtersArgs = map (splitOn ":") filters
        pipeline = mapMaybe (resolveFilter ctx' filters') filtersArgs
    in
        runFilters ctx' val pipeline


-- | Given a @[String]@ returns a tuple of (@filter@, @arg@)
--
-- Examples:
--
-- >>> import Tpll.Context (ctx, cStr, ContextValue(CStr))
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>> import Tpll.Tags.DefaultFilters (upperFilter)
--
-- >>> let ctx' = ctx [("bar", cStr "foo")]
-- >>> let Tags (_, filters) = getAllDefaultTags
-- >>>
-- >>> let Just (upperFilter, Just (CStr _ "foo")) = resolveFilter ctx' filters ["upper", "bar"]
resolveFilter :: Context -> FilterMap -> [String] -> Maybe (Filter, Maybe ContextValue)
resolveFilter ctx' filters' (filter:args) =
    case filter `lookup` filters' of
        Nothing ->
            Nothing
        Just f ->
            case args of
                (arg:_) ->
                    Just (f, resolveCtx ctx' arg)
                [] ->
                    Just (f, Nothing)


-- | Run filter pipeline over value
--
-- Examples:
--
-- >>> import Tpll.Context (ctx, ContextValue(CStr), cStr)
-- >>> import Tpll.Tags.Default (getAllDefaultTags)
-- >>> import Tpll.Tags.DefaultFilters (defaultFilter)
--
-- >>> let ctx' = ctx []
-- >>> let filters = [(defaultFilter, Just (cStr "bar"))]
-- >>> let Just (CStr _ r) = runFilters ctx' Nothing filters
-- >>> r
-- "bar"
--
-- -- >>> let ctx' = ctx []
-- -- >>> let filters = []
-- -- >>> let Just (CStr r) = runFilters ctx' (Just (CStr "foo")) filters
-- -- >>> r
-- -- "foo"
--
-- -- >>> let ctx' = ctx []
-- -- >>> let filters = [upperFilter, lowerFilter]
-- -- >>> let Just (CStr r) = runFilters ctx' (Just (CStr "fooBar")) filters
-- -- >>> r
-- -- "foobar"
runFilters :: Context -> Maybe ContextValue -> [(Filter, Maybe ContextValue)] -> Maybe ContextValue
runFilters ctx' val [] = val
runFilters ctx' val ((fn, arg):filters) =
    runFilters ctx' (fn ctx' val arg) filters


-- | Check if the given ContextValue can be considered True or False
--
-- Examples:
--
-- >>> import Tpll.Context (cStr)
--
-- >>> isFalse (Just (cStr ""))
-- True
-- >>> isFalse (Just (cStr " "))
-- False
-- >>> isFalse (Just (cInt 0))
-- True
-- >>> isFalse (Just (cInt 1))
-- False
-- >>> isFalse (Just (cInteger 0))
-- True
-- >>> isFalse (Just (cInteger 1))
-- False
-- >>> isFalse (Just (cDouble 0.0))
-- True
-- >>> isFalse (Just (cDouble 0.1))
-- False
-- >>> isFalse (Just (cList []))
-- True
-- >>> isFalse (Just (cList [cInt 1]))
-- False
isFalse :: Maybe ContextValue -> Bool
isFalse Nothing = True
isFalse (Just (CStr _ "")) = True
isFalse (Just (CInt _ 0)) = True
isFalse (Just (CInteger _ 0)) = True
isFalse (Just (CDouble _ 0.0)) = True
isFalse (Just (CList _ [])) = True
isFalse _ = False
