module Tpll.Context
(
    Context,
    ContextValue(CStr, CInt, CList, CAssoc),
    ctx
) where


import Prelude hiding (lookup)
import Data.Map.Strict (Map, fromList, lookup)


data ContextValue = CStr    String                          |
                    CInt    Int                             |
                    CAssoc  (ContextValue, ContextValue)    |
                    CList   [ContextValue]

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
ctx list =
    fromList list
