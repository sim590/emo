
module Utils where

{-|
   Format d'un fichier CSV d'emo décodé.
-}
type DecodedCsv = [(String, String)]

{-|
   Sélectionne l'emoji dans la liste des données décodées par l'analyseur du
   CSV.
-}
getEmoji :: DecodedCsv -> Int -> String
getEmoji emojis = snd . (emojis !!)

{-|
   Sélectionne l'information relative à l'emoji dans la liste des données
   décodées par l'analyseur du CSV.
-}
getEmojiInfo :: DecodedCsv -> Int -> String
getEmojiInfo emojis = fst . (emojis !!)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

