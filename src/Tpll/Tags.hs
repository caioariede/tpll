module Tpll.Tags
(
    Tag,
    Tags,
    TagAction(Render, RenderBlock),
    tags
) where


import Tpll.Tokenizer (Token)
import Tpll.Context (Context)

import Data.Map.Strict (Map, fromList)


data TagAction =
    Render ([Token], IO String)                         |
    RenderBlock ([Context], Context, [Token], Token)

type Tag = Context -> Token -> [Token] -> TagAction
type Tags = Map String Tag


tags :: [(String, Tag)] -> Tags
tags list =
    fromList list
