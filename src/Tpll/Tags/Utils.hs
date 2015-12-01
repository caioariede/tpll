{-|
Module      : Tpll.Tags.Utils
Description : Utility functions often used by template tags and filters
-}

module Tpll.Tags.Utils
(
    resolveParts,
    resolveValue
)
where


import Tpll.Context (Context, ContextValue(CInt), ctx, resolveCtx, ctxToString)
import Tpll.Tags (Tags(Tags), Filter, tags)


import Prelude hiding (lookup)
import Data.Map.Strict (lookup)
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)


-- | Resolve parts of a template tag
--
-- Examples:
--
-- >>> let ctx' = ctx [("a", CInt 1)]
-- >>> let tags' = tags [] []
-- >>> let [Just (CInt 1), Nothing] = resolveParts ctx' tags' ["a", "b"]
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
resolveValue :: Context -> Tags -> String -> Maybe ContextValue
resolveValue ctx' (Tags (_, filters')) value =
    let (key:filters) = splitOn "|" value
        val = resolveCtx ctx' key
        pipeline = mapMaybe (`lookup` filters') filters
    in
        runFilters ctx' val pipeline


-- | Run filter pipeline over value
--
-- Examples:
--
-- -- >>> let ctx' = ctx []
-- -- >>> let filters = [upperFilter]
-- -- >>> let Just (CStr r) = runFilters ctx' (Just (CStr "foo")) filters
-- -- >>> r
-- -- "FOO"
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
runFilters :: Context -> Maybe ContextValue -> [Filter] -> Maybe ContextValue
runFilters ctx' val [] = val
runFilters ctx' val (fn:filters) =
    runFilters ctx' (fn ctx' val) filters
