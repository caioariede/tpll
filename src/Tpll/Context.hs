module Tpll.Context
(
    Context,
    ContextValue(CStr, CInt, CDouble, CList, CAssoc),
    ctx,
    resolveCtx,
    ctxToString
) where


import Prelude hiding (lookup)
import Data.Map.Strict (Map, fromList, lookup)
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Text.Read (readMaybe)


data ContextValue = CStr        String                          |
                    CInt        Int                             |
                    CInteger    Integer                         |
                    CDouble     Double                          |
                    CAssoc      (ContextValue, ContextValue)    |
                    CList       [ContextValue]

                    deriving (Eq)

type Context = Map String ContextValue


-- | Build context
--
-- Examples:
--
-- >>> let foo = ctx [("foo", CStr "bar")]
-- >>> let Just (CStr r) = lookup "foo" foo
-- >>> r
-- "bar"
--
-- >>> let bar = ctx [("foo", CStr "bar"), ("bar", CInt 2)]
-- >>> let Just (CInt r) = lookup "bar" bar
-- >>> r
-- 2
ctx :: [(String, ContextValue)] -> Context
ctx = fromList


-- | Resolve context value
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> let Just (CStr r) = resolveCtx ctx' "\"foo\""
-- >>> r
-- "foo"
--
-- >>> let ctx' = ctx []
-- >>> let Nothing = resolveCtx ctx' "foo"
--
-- >>> let ctx' = ctx [("foo", CInt 42)]
-- >>> let Just (CInt r) = resolveCtx ctx' "foo"
-- >>> r
-- 42
resolveCtx :: Context -> String -> Maybe ContextValue
resolveCtx ctx' str =
    case head str of
        '"' ->
            Just (CStr (tail $ take (length str - 1) str))
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
-- >>> let Just (CInt r) = resolveCtxNumber ctx' "1"
-- >>> r
-- 1
--
-- >>> let ctx' = ctx []
-- >>> let Just (CDouble r) = resolveCtxNumber ctx' "1.2"
-- >>> r
-- 1.2
--
-- >>> let ctx' = ctx []
-- >>> let Just (CDouble r) = resolveCtxNumber ctx' "1.2e3"
-- >>> r
-- 1200.0
--
-- >>> let ctx' = ctx []
-- >>> let Nothing = resolveCtxNumber ctx' "1.2a"
resolveCtxNumber :: Context -> String -> Maybe ContextValue
resolveCtxNumber ctx' str =
    if "." `isInfixOf` str then
        case readMaybe str :: Maybe Double of
            Just number ->
                Just (CDouble number)
            _ ->
                Nothing
    else
        case readMaybe str :: Maybe Int of
            Just number ->
                Just (CInt number)
            _ ->
                Nothing


-- | ContextValue to String
--
-- Examples:
--
-- >>> ctxToString (Just (CStr "foo"))
-- "foo"
--
-- >>> ctxToString (Just (CInt 1))
-- "1"
--
-- >>> ctxToString (Just (CInteger 2))
-- "2"
--
-- >>> ctxToString (Just (CDouble 3.14))
-- "3.14"
ctxToString :: Maybe ContextValue -> String
ctxToString val =
    case val of
        Just (CStr x) ->
            x
        Just (CInt x) ->
            show x
        Just (CInteger x) ->
            show x
        Just (CDouble x) ->
            show x
        _ ->
            ""
