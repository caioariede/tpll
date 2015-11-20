{-# LANGUAGE ExistentialQuantification #-}

module Template where

import Prelude hiding (lookup)

import Data.Map.Strict (Map, fromList, lookup)
import Data.Array
import Text.Regex.PCRE
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad (liftM2)

type Context = Map String String

data Token =
    Tag         { content :: String, line :: Int }  |
    Text        { content :: String, line :: Int }  |
    Comment     { content :: String, line :: Int }  |
    Variable    { content :: String, line :: Int }
    deriving (Show)

type TagResult = ([Token], IO String)
type Tag = Context -> Token -> [Token] -> TagResult
type Tags = Map String Tag


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


tags :: [(String, Tag)] -> Tags
tags list =
    fromList list



-- | template tag: firstof
--
-- Examples:
--
-- >>> let ctx' = ctx [("a", ""), ("b", "x")]
-- >>> let (_, r) = firstOfTag ctx' (Tag { content = "firstof a b", line = 1 }) []
-- >>> r
-- "x"
--
-- >>> let (_, r) = firstOfTag ctx' (Tag { content = "firstof a c", line = 1 }) []
-- >>> r
-- ""
firstOfTag :: Context -> Token -> [Token] -> TagResult
firstOfTag ctx' token tokens =
    let Tag { content = content, line = _ } = token
        (_, parts) = splitAt 1 $ words content
    in
        (tokens, return $ firstOfTag' ctx' parts)

firstOfTag' :: Context -> [String] -> String
firstOfTag' _ [] = ""
firstOfTag' ctx' (x:xs) =
    case lookup x ctx' of
        Just x ->
            if x == "" then
                firstOfTag' ctx' xs
            else
                x
        Nothing ->
            firstOfTag' ctx' xs


-- | template tag: now
--
-- Examples:
--
-- >>> let ctx' = ctx []
-- >>> let expected = fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%I:%S") getCurrentTime
-- >>> let (_, r) = nowTag ctx' (Tag { content = "now", line = 1 }) []
-- >>> :{
--  do
--      a <- expected
--      b <- r
--      print $ show $ a == b
-- :}
-- "True"
nowTag :: Context -> Token -> [Token] -> TagResult
nowTag ctx' token tokens =
    let result = fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%I:%S") getCurrentTime
    in
        ([], result)


-- | Consume tag token
--
-- Examples:
--
-- >>> let ctx' = ctx [("a", ""), ("b", "x")]
-- >>> let tags' = tags [("firstof", firstOfTag)]
-- >>> let (_, r) = consumeTag ctx' tags' (Tag { content = "firstof a b", line = 1 }) []
-- >>> r
-- "x"
-- >>> let (_, r) = consumeTag ctx' tags' (Tag { content = "x", line = 1 }) []
-- >>> r
-- "<error>"
consumeTag :: Context -> Tags -> Token -> [Token] -> TagResult
consumeTag ctx' tags' token tokens =
    let Tag { content = content, line = _ } = token
        ([name], parts) = splitAt 1 $ words content
    in
        case lookup name tags' of
            Just fn -> fn ctx' token tokens
            Nothing ->
                (tokens, return "<error>")


-- | Parse tokens
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", "bar"), ("bar", show 2)]
-- >>> let tags' = tags []
-- >>> lookup "foo" ctx'
-- Just "bar"
-- >>> let [r] = parseTokens ctx' tags' [Variable { content = "foo", line = 1 }]
-- >>> r
-- "bar"
-- >>> let [r] = parseTokens ctx' tags' [Variable { content = "unknown", line = 1 }]
-- >>> r
-- ""
parseTokens :: Context -> Tags -> [Token] -> [IO String]
parseTokens ctx' tags' tokens =
    parseTokens' ctx' tags' tokens []

parseTokens' :: Context -> Tags -> [Token] -> [IO String] -> [IO String]
parseTokens' _ _ [] acc = reverse acc
parseTokens' ctx' tags' tokens acc =
    let (newtokens, result) = parseToken ctx' tags' tokens
    in
        parseTokens' ctx' tags' newtokens (result:acc)


-- | Parse token
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", "foo")]
-- >>> let tags' = tags []
-- >>> let (_, r) = parseToken ctx' tags' [Variable { content = "foo", line = 1 }]
-- >>> r
-- "foo"
parseToken :: Context -> Tags -> [Token] -> TagResult
parseToken ctx' tags' (token:tokens) =
    case token of
        Variable { content = content, line = _ } ->
            case lookup content ctx' of
                Just a -> (tokens, return a)
                _ -> (tokens, return "")
        Text { content = content, line = _ } ->
            (tokens, return content)
        Tag { content = _, line = _ } ->
            consumeTag ctx' tags' token tokens
        _ ->
            (tokens, return "")


-- | Parse string
--
-- Examples:
--
-- >>> let ctx' = ctx [("foo", "bar"), ("bar", show 2), ("bang", "")]
-- >>> let tags' = tags [("firstof", firstOfTag)]
-- >>> parseString ctx' tags' "abc{{ foo }}def{{ bar }}x"
-- "abcbardef2x"
--
-- >>> parseString ctx' tags' "foo{% firstof bang %}bar"
-- "foobar"
--
-- >>> parseString ctx' tags' "foo{% firstof bang bar %}bar"
-- "foo2bar"
parseString :: Context -> Tags -> String -> IO String
parseString ctx' tags' text =
    foldl1 (liftM2 (++)) $ parseTokens ctx' tags' $ tokenize text
