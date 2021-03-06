{-|
Module      : Tpll.Tags
-}

module Tpll.Tags
(
    Tag, TagMap,
    Filter, FilterMap,
    Tags(Tags),
    TagAction(Render, RenderBlock),
    tags
) where


import Tpll.Tokenizer (Token)
import Tpll.Context (Context, ContextValue)

import Data.Map.Strict (Map, fromList)


-- | The action returned by the template tags. It can be either `Render` or
-- `RenderBlock`.
--
-- The `Render` action returns a list containing the remaining tokens (those
-- not consumed by the parser) and the string to be rendered.
--
-- The `RenderBlock` action returns a context list, the old context, a list
-- of tokens to be rendered and the list of remaining tokens.
--
-- The context list returned by the `RenderBlock` action can be used by loop
-- tags, meaning that the block will be iterated over all contexts returned.
-- The "final" token is used to define where the block ends. For example, the
-- final token for the @{% for x in list %}@ tag is the @{% endfor %}@ tag.
data TagAction =
    Render ([Token], IO String)                         |
    RenderBlock ([Context], Context, [Token], [Token])


-- | Defines a template tag that accepts three arguments: `Context`, `Token`,
-- and the list of remaining tokens, not consumed by the template tag.
--
-- Returns a `TagAction`.
type Tag = Context -> Tags -> Token -> [Token] -> TagAction
type TagMap = Map String Tag


-- | Defines a template filter that accepts two arguments: `Context` and
-- `ContextValue`.
type Filter = Context -> Maybe ContextValue -> Maybe ContextValue -> Maybe ContextValue
type FilterMap = Map String Filter


-- | Defines two maps containing template tags (`Tag`) and filters (`Filter`).
data Tags = Tags (TagMap, FilterMap)


-- | Constructs the `Tags` containing template tags and filters.
tags :: [(String, Tag)] -> [(String, Filter)] -> Tags
tags tags' filters' =
    Tags (fromList tags', fromList filters')
