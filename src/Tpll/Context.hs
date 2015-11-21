module Tpll.Context
(
    Context,
    ctx
) where


import Prelude hiding (lookup)
import Data.Map.Strict (Map, fromList, lookup)


type Context = Map String String


-- | Build context
--
-- Examples:
--
-- >>> let foo = ctx [("foo", "bar")]
-- >>> lookup "foo" foo
-- Just "bar"
--
-- >>> let bar = ctx [("foo", "bar"), ("bar", show 2)]
-- >>> lookup "bar" bar
-- Just "2"
ctx :: [(String, String)] -> Context
ctx list =
    fromList list
