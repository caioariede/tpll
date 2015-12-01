{-|
Module      : Tpll.Tokenizer
Description : Functions to transform strings in tokens
-}

module Tpll.Tokenizer
(
    tokenize,
    Token(Tag, Text, Comment, Variable, content, line, raw),
) where


import Text.Regex.PCRE ((=~), MatchOffset, MatchLength)


-- | All possible token types that can be generated by the `tokenize` function.
data Token =
    Tag         { content :: String, line :: Int, raw :: String }  |
    Text        { content :: String, line :: Int, raw :: String }  |
    Comment     { content :: String, line :: Int, raw :: String }  |
    Variable    { content :: String, line :: Int, raw :: String }

    deriving (Show, Eq)


regexTag :: String
regexTag = "({%.*?%}|{{.*?}}|{#.*?#})"


-- | Count line breaks in text
--
-- Examples:
--
-- >>> countLineBreaks "foo\nbar\r\nbang"
-- 2
countLineBreaks :: String -> Int
countLineBreaks text =
    countLineBreaksT text 0

countLineBreaksT :: String -> Int -> Int
countLineBreaksT [] count = count
countLineBreaksT (x:y:xs) count
    | x == '\n' =
        countLineBreaksT (y:xs) (count + 1)
    | x == '\r' && y == '\n' =
        countLineBreaksT xs (count + 1)
    | otherwise =
        countLineBreaksT (y:xs) count
countLineBreaksT (x:xs) count
    | x == '\n' =
        countLineBreaksT xs (count + 1)
    | otherwise =
        countLineBreaksT xs count
    

-- | Text inside tag
--
-- Examples:
--
-- >>> tagText "{% foo %}"
-- "foo"
tagText :: String -> String
tagText text =
    let trim = (unwords . words)
    in
        trim $ drop 2 $ take (length text - 2) text


-- | Find next parsing position
--
-- Examples:
--
-- >>> nextPos ""
-- Nothing
--
-- >>> nextPos "foo"
-- Nothing
--
-- >>> nextPos "foo{{ test }}"
-- Just (3,13)
--
-- >>> nextPos "{% test %}bar"
-- Just (0,10)
nextPos :: String -> Maybe (Int, Int)
nextPos "" = Nothing
nextPos text =
    let matches = text =~ regexTag :: (MatchOffset, MatchLength)
        (offset, len) = matches
    in
        if offset < 0 then
            Nothing
        else
            Just (offset, offset + len)


-- | Tokenize template string
--
-- Examples:
--
-- >>> tokenize "foo\n{{ test }}\r\nbar"
-- [Text {content = "foo\n", line = 1, raw = ""},Variable {content = "test", line = 2, raw = "{{ test }}"},Text {content = "\r\nbar", line = 3, raw = ""}]
--
-- >>> tokenize "foo{# test a #}"
-- [Text {content = "foo", line = 1, raw = ""},Comment {content = "test a", line = 1, raw = "{# test a #}"}]
--
-- >>> tokenize "foo\n{% foo %}"
-- [Text {content = "foo\n", line = 1, raw = ""},Tag {content = "foo", line = 2, raw = "{% foo %}"}]
--
-- >>> tokenize "a{{ b }}c{{ d }}"
-- [Text {content = "a", line = 1, raw = ""},Variable {content = "b", line = 1, raw = "{{ b }}"},Text {content = "c", line = 1, raw = ""},Variable {content = "d", line = 1, raw = "{{ d }}"}]
tokenize :: String -> [Token]
tokenize text =
    tokenize' text 1 []


tokenize' :: String -> Int -> [Token] -> [Token]
tokenize' str ln tokens =
    case nextPos str of
        Nothing ->
            if str == "" then
                reverse tokens
            else
                let newline = ln + countLineBreaks str
                in
                    reverse (getTextToken str newline : tokens)

        Just (first, lst) ->
            if first > 0 then
                let (h, rest) = splitAt first str
                    newline = ln + countLineBreaks h
                in
                    tokenize' rest newline (getTextToken h ln:tokens)
            else
                let (h, rest) = splitAt lst str
                    newline = ln + countLineBreaks h
                in
                    case getToken h ln of
                        Nothing ->
                            tokenize' rest newline tokens
                        Just token ->
                            tokenize' rest newline (token:tokens)


-- | Get text token
--
-- Examples:
--
-- >>> getTextToken "foobar" 2
-- Text {content = "foobar", line = 2, raw = ""}
getTextToken :: String -> Int -> Token
getTextToken str ln =
    Text { content = str, line = ln, raw = "" }


-- | Get a token for the given string
--
-- Examples:
--
-- >>> getToken "{{ foo }}" 1
-- Just (Variable {content = "foo", line = 1, raw = "{{ foo }}"})
--
-- >>> getToken "{# bar #}" 1
-- Just (Comment {content = "bar", line = 1, raw = "{# bar #}"})
--
-- >>> getToken "{% x y z %}" 1
-- Just (Tag {content = "x y z", line = 1, raw = "{% x y z %}"})
getToken :: String -> Int -> Maybe Token
getToken text ln =
    case take 2 text of
        "{{" ->
            Just Variable { content = tagText text, line = ln, raw = text }
        "{#" ->
            Just Comment { content = tagText text, line = ln, raw = text }
        "{%" ->
            Just Tag { content = tagText text, line = ln, raw = text }
        _ ->
            Nothing
