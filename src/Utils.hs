
{-# LANGUAGE OverloadedStrings #-}

module Utils where


import Control.Monad.State
import Control.Monad.Reader

import Control.Lens

import Data.Text
import Data.Text.Read

import Csv (DecodedCsv)

readDecimal :: Text -> Int
readDecimal t = decimal t ^?! _Right ^. _1

{-|
   Sélectionne l'emoji dans la liste des données décodées par l'analyseur du
   CSV.
-}
getEmoji :: DecodedCsv -> Int -> Text
getEmoji emojis = snd . (emojis !!)

{-|
   Sélectionne l'information relative à l'emoji dans la liste des données
   décodées par l'analyseur du CSV.
-}
getEmojiInfo :: DecodedCsv -> Int -> Text
getEmojiInfo emojis = fst . (emojis !!)

{-| Texte d'aide pour les touches au menu interactif.
-}
keyMapsHelpText :: [Text]
keyMapsHelpText = [
                   "CTRL+H:    affiche ce texte d'aide."
                 , "CTRL+Y:    efface l'entrée et copie l'emoji associé au choix si l'entrée est valide."
                 , "CTRL+I:    affiche de l'information sur l'emoji."
                 , "CTRL+T:    filtre tous les emojis à l'aide de fzf."
                 , "CTRL+R:    choix aléatoire d'un emoji et le copie dans la presse-papier."
                 , "CTRL+P"
                 , "Haut:      remonte l'historique."
                 , "CTRL+N"
                 , "Bas:       redescend l'historique."
                 , "CTRL+L:    redessine l'écran."
                 , "CTRL+B"
                 , "Gauche:    déplace le curseur vers la gauche."
                 , "CTRL+F"
                 , "Droite:    déplace le curseur vers la droite."
                 , "CTRL+A:    déplace le curseur au début de la ligne."
                 , "CTRL+E:    déplace le curseur à la fin de la ligne."
                 , "CTRL+D:    supprime un caractère sous le curseur."
                 , "Backspace: efface un caractère."
                 ]

liftRST :: Monad m => m a -> ReaderT r (StateT s m) a
liftRST = Control.Monad.Reader.lift . Control.Monad.State.lift

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

