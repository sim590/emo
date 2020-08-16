
module Utils where

import Control.Monad.State
import Control.Monad.Reader

import Csv (DecodedCsv)

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

{-| Texte d'aide pour les touches au menu interactif.
-}
keyMapsHelpText :: [String]
keyMapsHelpText = [
                   "CTRL+H:    affiche ce texte d'aide."
                 , "CTRL+Y:    efface l'entrée et copie l'emoji associé au choix si l'entrée est valide."
                 , "CTRL+I:    affiche de l'information sur l'emoji."
                 , "CTRL+R:    choix aléatoire d'un emoji et le copie dans la presse-papier."
                 , "CTRL+L:    redessine l'écran."
                 , "CTRL+B:    déplace le curseur vers la gauche."
                 , "CTRL+F:    déplace le curseur vers la droite."
                 , "CTRL+A:    déplace le curseur au début de la ligne."
                 , "CTRL+E:    déplace le curseur à la fin de la ligne."
                 , "CTRL+D:    supprime un caractère sous le curseur."
                 , "Backspace: efface un caractère."
                 ]

liftRST :: Monad m => m a -> ReaderT r (StateT s m) a
liftRST = Control.Monad.Reader.lift . Control.Monad.State.lift

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

