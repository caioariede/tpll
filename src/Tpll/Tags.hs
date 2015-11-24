module Tpll.Tags
(
    Tag,
    Filter,
    Tags,
    TagAction(Render, RenderBlock),
    tags
) where


import Tpll.Tokenizer (Token)
import Tpll.Context (Context, ContextValue)

import Data.Map.Strict (Map, fromList)


data TagAction =
    Render ([Token], IO String)                         |
    RenderBlock ([Context], Context, [Token], Token)

type Tag = Context -> Token -> [Token] -> TagAction
type Filter = Context -> Maybe ContextValue -> Maybe ContextValue
type Tags = (Map String Tag, Map String Filter)


tags :: [(String, Tag)] -> [(String, Filter)] -> Tags
tags tags' filters' =
    (fromList tags', fromList filters')
