{-|
Module      : Tpll.Tags.Utils
-}

module Tpll.Tags.Utils
(
    resolveParts
)
where


import Tpll.Context (Context, ContextValue(CInt), ctx, resolveCtx)


-- | Resolve parts of a template tag
--
-- Examples:
--
-- >>> let ctx' = ctx [("a", CInt 1)]
-- >>> let [Just (CInt 1), Nothing] = resolveParts ctx' ["a", "b"]
resolveParts :: Context -> [String] -> [Maybe ContextValue]
resolveParts ctx' =
    map (resolveCtx ctx')
