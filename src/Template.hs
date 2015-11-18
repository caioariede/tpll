{-# LANGUAGE ExistentialQuantification #-}

module Template where

import Prelude hiding (lookup)

import Data.Map.Strict (Map, fromList, lookup)
import Data.Array
import Text.Regex.PCRE


data Showable = forall a . (Show a) => Showable a

instance Show Showable where
    show (Showable a) = show a

type Context = Map String Showable

data Token =
    Tag         { content :: String, line :: Int }  |
    Text        { content :: String, line :: Int }  |
    Comment     { content :: String, line :: Int }  |
    Variable    { content :: String, line :: Int }
    deriving (Show)


regexTag = "({%.*?%}|{{.*?}}|{#.*?#})" :: String


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


-- | Tokenize template string
--
-- Examples:
--
-- >>> tokenize "foo\n{{ test }}\r\nbar"
-- [Text {content = "foo\n", line = 1},Variable {content = "test", line = 2},Text {content = "\r\nbar", line = 3}]
--
-- >>> tokenize "foo{# test a #}"
-- [Text {content = "foo", line = 1},Comment {content = "test a", line = 1}]
--
-- >>> tokenize "foo\n{% foo %}"
-- [Text {content = "foo\n", line = 1},Tag {content = "foo", line = 2}]
--
-- >>> tokenize "a{{ b }}c{{ d }}"
-- [Text {content = "a", line = 1},Variable {content = "b", line = 1},Text {content = "c", line = 1},Variable {content = "d", line = 1}]
tokenize :: String -> [Token]
tokenize text =
    tokenizeAcc text 1 []


tokenizeAcc :: String -> Int -> [Token] -> [Token]
tokenizeAcc str line tokens =
    case nextPos str of
        Nothing ->
            if str == "" then
                reverse tokens
            else
                let newline = line + (countLineBreaks str)
                in
                    reverse ((getTextToken str newline):tokens)

        Just (first, last) ->
            if first > 0 then
                let (head, rest) = splitAt first str
                    newline = line + (countLineBreaks head)
                in
                    tokenizeAcc rest newline ((getTextToken head line):tokens)
            else
                let (head, rest) = splitAt last str
                    newline = line + (countLineBreaks head)
                in
                    case getToken head line of
                        Nothing ->
                            tokenizeAcc rest newline tokens
                        Just token ->
                            tokenizeAcc rest newline (token:tokens)


-- | Get text token
--
-- Examples:
--
-- >>> getTextToken "foobar" 2
-- Text {content = "foobar", line = 2}
getTextToken :: String -> Int -> Token
getTextToken str line =
    Text { content = str, line = line }


-- | Get a token for the given string
--
-- Examples:
--
-- >>> getToken "{{ foo }}" 1
-- Just (Variable {content = "foo", line = 1})
--
-- >>> getToken "{# bar #}" 1
-- Just (Comment {content = "bar", line = 1})
--
-- >>> getToken "{% x y z %}" 1
-- Just (Tag {content = "x y z", line = 1})
getToken :: String -> Int -> Maybe Token
getToken text line =
    case take 2 text of
        "{{" ->
            Just Variable { content = tagText text, line = line }
        "{#" ->
            Just Comment { content = tagText text, line = line }
        "{%" ->
            Just Tag { content = tagText text, line = line }
        _ ->
            Nothing
    

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
        trim $ drop 2 $ take ((length text) - 2) $ text


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
            Just (offset, (offset + len))


-- | Build context
--
-- Examples:
--
-- >>> let foo = buildContext [("foo", Showable "bar")]
-- >>> lookup "foo" foo
-- Just "bar"
--
-- >>> let bar = buildContext [("foo", Showable "bar"), ("bar", Showable 2)]
-- >>> lookup "bar" bar
-- Just 2
buildContext :: [(String, Showable)] -> Context
buildContext list =
    fromList list


-- | Parse tokens
--
-- Examples:
--
-- >>> let ctx = buildContext [("foo", Showable "bar"), ("bar", Showable 2)]
-- >>> lookup "foo" ctx
-- Just "bar"
-- >>> parseTokens ctx [Variable { content = "foo", line = 1 }]
-- ["bar"]
-- >>> parseTokens ctx [Variable { content = "unknown", line = 1 }]
-- [""]
parseTokens :: Context -> [Token] -> [String]
parseTokens ctx tokens =
    map (parseToken ctx) tokens


-- | Parse token
--
-- Examples:
--
-- >>> let ctx = buildContext [("foo", Showable "foo")]
-- >>> parseToken ctx Variable { content = "foo", line = 1 }
-- "foo"
parseToken :: Context -> Token -> String
parseToken ctx token =
    case token of
        Variable { content = content, line = _ } ->
            case lookup content ctx of
                Just a -> show a
                _ -> ""
        Text { content = content, line = _ } ->
            content
        _ ->
            ""


-- | Parse string
--
-- Examples:
--
-- >>> let ctx = buildContext [("foo", Showable "bar"), ("bar", Showable 2)]
-- >>> parseString ctx "abc{{ foo }}def{{ bar }}x"
-- "abcbardef2x"
parseString :: Context -> String -> String
parseString ctx text =
    concat $ parseTokens ctx $ tokenize text
