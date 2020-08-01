
module Utils where

type DecodedCsv = [(String, String)]

getEmoji :: DecodedCsv -> Int -> String
getEmoji emojis = snd . (emojis !!)

getEmojiInfo :: DecodedCsv -> Int -> String
getEmojiInfo emojis = fst . (emojis !!)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

