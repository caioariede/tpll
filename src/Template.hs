module Template where


import qualified Data.Text as T

import Data.Array
import Text.Regex.PCRE


data Token =
    Text        { content :: String, line :: Int }                  |
    Block       { content :: String, line :: Int, name :: String }  |
    Comment     { content :: String, line :: Int }                  |
    Variable    { content :: String, line :: Int }
    deriving (Show)


regexTag = "({%.*?%}|{{.*?}}|{#.*?#})" :: String


-- | Tokenize template string
--
-- Examples:
--
-- >>> tokenize "foo{{ test }}bar"
-- [Text {content = "foo", line = 1},Variable {content = "test", line = 1},Text {content = "bar", line = 1}]
tokenize :: String -> [Token]
tokenize text =
    tokenizeAcc text []


tokenizeAcc :: String -> [Token] -> [Token]
tokenizeAcc str tokens =
    case nextPos str of
        Nothing ->
            if str == "" then
                reverse tokens
            else
                let token = Text { content = str, line = 1 }
                in
                    reverse (token:tokens)

        Just (first, last) ->
            let text = T.pack str
                in
                    if first > 0 then
                        let (head, rest) = T.splitAt first text
                            reststr = T.unpack rest
                            headstr = T.unpack head
                            node = Text { content = headstr, line = 1 }
                        in
                            tokenizeAcc reststr (node:tokens)
                    else
                        let (head, rest) = T.splitAt last text
                            reststr = T.unpack rest
                        in
                            case getToken head 1 of
                                Nothing ->
                                    tokenizeAcc reststr tokens
                                Just token ->
                                    tokenizeAcc reststr (token:tokens)


-- | Get a token for the given string
--
-- Examples:
--
-- >>> getToken (T.pack "{{ foo }}") 1
-- Just (Variable {content = "foo", line = 1})
getToken :: T.Text -> Int -> Maybe Token
getToken text line =
    if T.pack "{{" `T.isPrefixOf` text then
        Just Variable { content = T.unpack $ tagText text, line = line }
    else
        Nothing
    

-- | Text inside tag
--
-- Examples:
--
-- >>> tagText (T.pack "{% foo %}")
-- "foo"
tagText :: T.Text -> T.Text
tagText text =
    T.strip $ T.drop 2 $ T.dropEnd 2 $ text


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
