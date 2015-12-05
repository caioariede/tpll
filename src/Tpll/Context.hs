{-|
Module      : Tpll.Context
Description : Functions to encapsulate values into contexts
-}

module Tpll.Context
(
    Context,
    ContextValue(CStr, CInt, CDouble, CList, CAssoc, is_safe),
    cStr, cInt, cInteger, cDouble, cAssoc, cList,
    ctx,
    resolveCtx,
    ctxToString
) where


import Prelude hiding (lookup)
import Data.Map.Strict (Map, fromList, lookup)
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Text.Read (readMaybe)
import Text.HTML.TagSoup.Entity (escapeXML)


-- | All possible values that can be passed to the context.
--
-- It's also possible to group values by using `CAssoc` and `CList`:
--
-- @
-- CAssoc (cInt 1, cStr "foo")
-- @
--
-- @
-- CList [cInt 1, cStr "foo", cAssoc (cInt 2, cStr "bar")]
-- @
data ContextValue =
    CStr        { is_safe :: Bool, str      :: String                       } |
    CInt        { is_safe :: Bool, int      :: Int                          } |
    CInteger    { is_safe :: Bool, integer  :: Integer                      } |
    CDouble     { is_safe :: Bool, double   :: Double                       } |
    CAssoc      { is_safe :: Bool, assoc    :: (ContextValue, ContextValue) } |
    CList       { is_safe :: Bool, list     :: [ContextValue]               }

    deriving (Eq)


cStr :: String -> ContextValue
cStr = CStr False


cInt :: Int -> ContextValue
cInt = CInt False


cInteger :: Integer -> ContextValue
cInteger = CInteger False


cDouble :: Double -> ContextValue
cDouble = CDouble False


cAssoc :: (ContextValue, ContextValue) -> ContextValue
cAssoc = CAssoc False


cList :: [ContextValue] -> ContextValue
cList = CList False


-- | A HashMap that holds the context that is passed to the parser
--
-- You can generate a context by using the `ctx` function:
--
-- @
-- let ctx' = ctx [("foo", cInt 1), ("bar", cStr "2")]
-- @
type Context = Map String ContextValue


-- | Helper function to create a Context HashMap from a list.
--
-- Examples:
--
-- >>> let foo = ctx [("foo", cStr "bar")]
-- >>> let Just (CStr False r) = lookup "foo" foo
-- >>> r
-- "bar"
--
-- >>> let bar = ctx [("foo", cStr "bar"), ("bar", cInt 2)]
-- >>> let Just (CInt False r) = lookup "bar" bar
-- >>> r
-- 2
ctx :: [(String, ContextValue)] -> Context
ctx = fromList


-- | Function used by template tags to resolve values passed as arguments.
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> let Just (CStr False r) = resolveCtx ctx' "\"foo\""
-- >>> r
-- "foo"
--
-- >>> let ctx' = ctx []
-- >>> let Nothing = resolveCtx ctx' "foo"
--
-- >>> let ctx' = ctx [("foo", cInt 42)]
-- >>> let Just (CInt False r) = resolveCtx ctx' "foo"
-- >>> r
-- 42
resolveCtx :: Context -> String -> Maybe ContextValue
resolveCtx ctx' str =
    case head str of
        '"' ->
            Just (CStr False (tail $ take (length str - 1) str))
        other ->
            if isDigit other then
                resolveCtxNumber ctx' str
            else
                lookup str ctx'


-- | Resolve context number value
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> let Just (CInt False r) = resolveCtxNumber ctx' "1"
-- >>> r
-- 1
--
-- >>> let ctx' = ctx []
-- >>> let Just (CDouble False r) = resolveCtxNumber ctx' "1.2"
-- >>> r
-- 1.2
--
-- >>> let ctx' = ctx []
-- >>> let Just (CDouble False r) = resolveCtxNumber ctx' "1.2e3"
-- >>> r
-- 1200.0
--
-- >>> let ctx' = ctx []
-- >>> let Nothing = resolveCtxNumber ctx' "1.2a"
resolveCtxNumber :: Context -> String -> Maybe ContextValue
resolveCtxNumber _ str =
    if "." `isInfixOf` str then
        case readMaybe str :: Maybe Double of
            Just number ->
                Just (CDouble False number)
            _ ->
                Nothing
    else
        case readMaybe str :: Maybe Int of
            Just number ->
                Just (CInt False number)
            _ ->
                Nothing


-- | Function used to convert a ContextValue into a String.
--
-- Examples:
--
-- >>> ctxToString (Just (cStr "foo"))
-- "foo"
--
-- >>> ctxToString (Just (cInt 1))
-- "1"
--
-- >>> ctxToString (Just (cInteger 2))
-- "2"
--
-- >>> ctxToString (Just (cDouble 3.14))
-- "3.14"
ctxToString :: Maybe ContextValue -> String
ctxToString val =
    case val of
        Just (CStr True x) ->
            x
        Just (CStr False x) ->
            escapeXML x
        Just (CInt _ x) ->
            show x
        Just (CInteger _ x) ->
            show x
        Just (CDouble _ x) ->
            show x
        _ ->
            ""
