module Tpll.Tags
(
    Tag,
    Tags,
    TagResult,
    tags
) where


import Tpll.Tokenizer (Token)
import Tpll.Context (Context)

import Data.Map.Strict (Map, fromList)


type TagResult = ([Token], IO String)
type Tag = Context -> Token -> [Token] -> TagResult
type Tags = Map String Tag


tags :: [(String, Tag)] -> Tags
tags list =
    fromList list
