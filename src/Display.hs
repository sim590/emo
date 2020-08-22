
{-# LANGUAGE TemplateHaskell #-}

module Display (
  emojiMenu
) where

import Data.List
import Data.Digits
import Data.Char
import Data.Maybe

import Text.Read

import Control.Monad
import Control.Monad.Morph
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Loops

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as ST

import Control.Lens

import System.Random

import Fzf
import Clip
import Utils
import Csv (DecodedCsv)

import UI.NCurses

data DisplayState = DisplayState { _helpPad     :: Maybe Pad
                                 , _inputString :: String
                                 , _emojis      :: DecodedCsv
                                 , _prompt      :: String
                                 }
makeLenses ''DisplayState

colOs :: Integer
colOs = 2

{-| Fournit le caractère correspondant à la combinaison de touche CTRL et le
   caractère d'entrée.
-}
ctrlKey :: Char -> Char
ctrlKey c = chr (ord c - 96)

{-| Message s'affichant lorsque la liste des choix est tronquée.
-}
truncatedMsg :: String
truncatedMsg = "// Certains choix ont été tronqués ... //"

displayPrompt :: Int -> String
displayPrompt len = "Choix (valeurs entre 1 et "++ show len ++ ") ? "

{-| Le nombre maximal de colonnes.
-}
maxColCount :: Integer -> [(String, String)] -> Integer
maxColCount w es = w `div` spacedColWidth w es

{-| Largeur maximal d'un index considérant le nombre maximal d'indexes.
-}
maxIWidth :: [(String, String)] -> Integer
maxIWidth es = fromIntegral $ length $ digits 10 $ length es

{-| Largeur d'une colonne en considérant la largeur maximale d'une colonne, la
   largeur maximal de l'index et les espaces blancs pour alignement des indexes.
-}
spacedColWidth :: Integer -> [(String, String)] -> Integer
spacedColWidth w es = maxColWidth w es + maxIWidth es + colOs

{-| La largeur maximale d'une colonne considérant la largeur de chacun des
   emojis.
-}
maxColWidth :: Integer -> [(String, String)] -> Integer
maxColWidth winWmax es = min winWmax (eWmax es)
  where comprule (_, e1) (_, e2) = compare (length e1) (length e2)
        eWmax = fromIntegral . length . snd . maximumBy comprule

{-| Le nombre maximal d'emoji pouvant s'afficher sur l'écran.
-}
maxEntryCount :: Integer -> Integer -> [(String, String)] -> Int
maxEntryCount w h es = fromInteger $ (h-3) * maxColCount w es

{-| Affiche une chaîne de caractère avec un attribut donné.
-}
drawStringWithAttr :: Attribute -> String -> Update ()
drawStringWithAttr attr s = setAttribute attr True >> drawString s >> setAttribute attr False

{-| Affiche du texte en caractères gras.
-}
drawBoldString :: String -> Update ()
drawBoldString = drawStringWithAttr AttributeBold

{-| Affiche le texte d'aide, c.-à-d. l'aide-mémoire des touches.
-}
showHelp :: StateT DisplayState Curses ()
showHelp = do
  (h, w) <- ST.lift screenSize
  let biggestWidth = maximum (map length keyMapsHelpText)
      pWidth       = min w $ fromIntegral biggestWidth + 4
      pHeight      = min h $ fromIntegral (length keyMapsHelpText) + 3
      padY         = div (h - pHeight) 2
      padX         = div (w - pWidth) 2
  mOldPad    <- use helpPad
  jpad <- case mOldPad of
    jop@(Just _) -> return jop
    _            -> ST.lift $ Just <$> newPad pHeight pWidth
  helpPad .= jpad
  let osWidth len = div (fromInteger pWidth - len) 2
      centered s  = replicate (osWidth $ length s) ' ' ++ s
      header      = centered "Aide-mémoire" ++ "\n"
      hpad        = fromJust jpad
  ST.lift $ updatePad hpad 0 0 padY padX h w $ do
    clear
    drawString "\n"       -- passer première ligne pour la bordure ...
    drawBoldString header
  ST.lift $ updatePad hpad 0 0 padY padX h w $ forM_ keyMapsHelpText $ \ l ->
    drawString $ replicate (osWidth biggestWidth) ' ' ++ l ++ "\n"
  ST.lift $ updatePad hpad 0 0 padY padX h w $ drawBox Nothing Nothing

filterEmojis :: DecodedCsv -> MaybeT (StateT DisplayState Curses) DecodedCsv
filterEmojis all_emojis = do
  next_emojis <- hoist liftIO $ fzf all_emojis
  emojis .= next_emojis
  prompt .= displayPrompt (length next_emojis)
  return next_emojis

{-| Affiche les choix d'emojis à l'écran.
-}
writeChoices :: DecodedCsv -> Update ()
writeChoices es = do
  (h, w) <- windowSize
  let mcc = maxColCount w es
      scw = spacedColWidth w es
      drawIthEmoji i (_, e) = do
        let (y, x) = (i `div` mcc, (i `mod` mcc) * (scw + colOs))
        moveCursor y x
        drawString $ show (i+1) ++ ") " ++ e
        return $ i + 1
  foldM_ drawIthEmoji 0 $ take (maxEntryCount w h es) es
  (y, _) <- cursorPosition
  moveCursor (y+2) 0

{-| Détermine si le choix est valide.

   * ch_str: (String) le choix.
   * n: (Integer) le nombre de choix totaux.
-}
validChoice :: String -> Integer -> Bool
validChoice ch_str n = case readMaybe ch_str of
  Just c -> 1 >= c || c <= n
  _      -> False

{-| Effectue une suppression d'un caractère à gauche du cruseur lorsque le
   celui-ci se trouve sur la ligne d'entrée usager.
-}
doBackspace :: StateT DisplayState Curses ()
doBackspace = do
  the_prompt <- use prompt
  istr       <- use inputString
  win        <- ST.lift defaultWindow
  (y, x)     <- ST.lift $ getCursor win
  let i          = fromInteger x - length the_prompt
      x0         = fromIntegral $ length the_prompt
      (beg, end) = splitAt i istr
      resul      = init beg ++ end
  when (x > x0) $ do
    inputString .= resul
    ST.lift $ updateWindow win $ do
      moveCursor y x0
      clearLine
      drawString resul
      moveCursor y (x-1)

{-| Boucle sur les événements du clavier et effectue les actions appropriées.

   * CTRL+H: affiche un texte d'aide.
   * CTRL+Y: efface l'entrée et copie l'emoji associé au choix si l'entrée est valide.
   * CTRL+I: affiche de l'information sur l'emoji.
   * CTRL+R: choix aléatoire d'un emoji et le copie dans la presse-papier.
   * CTRL+L: redessine l'écran.
   * CTRL+B: déplace le curseur vers la gauche.
   * CTRL+F: déplace le curseur vers la droite.
   * CTRL+A: déplace le curseur au début de la ligne.
   * CTRL+E: déplace le curseur à la fin de la ligne.
   * CTRL+D: supprime un caractère sous le curseur.
   * CTRL+T: filtre la liste d'emojis avec FZF.
   * Backspace: efface un caractère.
-}
handleEvents :: (Event -> Bool) -> ReaderT DecodedCsv (StateT DisplayState Curses) Event
handleEvents p = do
  all_emojis <- ask
  R.lift $ iterateUntil p $ do
    the_prompt <- use prompt
    let x0     = fromIntegral $ length the_prompt
        xmax s = x0 + fromIntegral (length s)
    win    <- ST.lift defaultWindow
    jev    <- ST.lift $ getEvent win Nothing
    (y, x) <- ST.lift $ getCursor win
    let updateW    = ST.lift . updateWindow win
        clearInput = updateW (moveCursor y x0 >> clearLine) >> inputString .= ""
        moveRight  = do
          s <- use inputString
          when (x < xmax s) $ updateW $ moveCursor y (x+1)
        moveLeft = when (x > x0) $ updateW $ moveCursor y (x-1)
    the_emojis <- use emojis
    ev  <- case jev of
      Just e@(EventCharacter '\n')           -> return e
      Just e@(EventSpecialKey KeyLeftArrow)  -> moveLeft    >> return e
      Just e@(EventSpecialKey KeyRightArrow) -> moveRight   >> return e
      Just e@(EventSpecialKey KeyBackspace)  -> doBackspace >> return e
      Just e@(EventCharacter c) -> do
        s <- use inputString
        let n = fromIntegral $ length the_emojis
            emoji i         = getEmoji the_emojis (i-1)
            emojiInfo i     = getEmojiInfo the_emojis (i-1)
            emojiPlusInfo i = emoji i ++ " " ++ "(" ++ emojiInfo i ++ ")"
            clearInputAndCopy i = do
              clearInput
              ST.lift $ liftIO $ copyToClipBoard $ emoji i
              updateW $ drawBottomInfo $ emojiPlusInfo i ++ " copié dans le presse-papier..."
        if      c == ctrlKey 'y' then when (validChoice s n) $ clearInputAndCopy $ read s
        else if c == ctrlKey 'h' then showHelp
        else if c == ctrlKey 't' then do
          mfzfed_emojis <- runMaybeT $ filterEmojis all_emojis
          redrawMenu
          when (isNothing mfzfed_emojis) $ updateW $ drawBottomInfo "erreur: fzf est introuvable..."
        else if c == ctrlKey 'i' then when (validChoice s n) $ do
          let i = read s
          updateW $ drawBottomInfo $ emojiPlusInfo i
        else if c == ctrlKey 'r' then do
          i <- ST.lift $ liftIO $ randomRIO (1, length the_emojis - 1)
          clearInputAndCopy i
        -- Quelques touches de readline
        else if c == ctrlKey 'l' then redrawMenu
        else if c == ctrlKey 'u' then clearInput
        else if c == ctrlKey 'd' then when (x < xmax s) $ moveRight >> doBackspace
        else if c == ctrlKey 'b' then moveLeft
        else if c == ctrlKey 'f' then moveRight
        else if c == ctrlKey 'a' then updateW $ moveCursor y x0
        else if c == ctrlKey 'e' then updateW $ moveCursor y $ xmax s
        -- On affiche tout autre caractère à la ligne.
        else do
          updateW $ drawString [c]
          inputString .= s ++ [c]
        return e
      Just e -> return e
      _      -> return $ EventUnknown 0
    ST.lift render
    return ev

{-| Dessine un message d'information au bas de l'écran. Le curseur est
   repositionné à son emplacement de départ.

   Hypothèse: le curseur Curses se trouve au niveau de la ligne d'entrée usager.
-}
drawBottomInfo :: String -> Update ()
drawBottomInfo infos = do
  (y, x) <- cursorPosition
  moveCursor (y+1) 0
  clearLine
  drawString $ "`--> " ++ infos
  moveCursor y x

{-| Dessine l'invite d'entrée pour l'utilisateur.

   Hypothèse: le curseur Curses se trouve au niveau de la ligne d'entrée usager.
-}
drawInputTitle :: String -> Int -> Int -> Update ()
drawInputTitle title n mec = do
  (y, _) <- cursorPosition

  moveCursor (y-1) 0
  clearLine
  unless (n <= mec) $ drawString truncatedMsg

  moveCursor y 0
  clearLine
  drawString title

{-| Redessine le menu.

   Cette fonction est normalement appelée lorsque le terminal est redimensionné.
-}
redrawMenu :: StateT DisplayState Curses ()
redrawMenu = do
  win        <- ST.lift defaultWindow
  inputstr   <- use inputString
  esl        <- use emojis
  the_prompt <- use prompt
  ST.lift $ updateWindow win $ do
    clear
    (h, w) <- windowSize
    let n   = length esl
        mec = maxEntryCount w h esl
        scw = fromIntegral $ spacedColWidth w esl
    if fromIntegral w >= maximum [scw, length the_prompt, length truncatedMsg] && h >= 3 then
      writeChoices esl >> drawInputTitle the_prompt n mec >> drawString inputstr
    else let w_too_small = "err: Fenêtre trop petite..." in
             when (w >= fromIntegral (length w_too_small)) $ drawString w_too_small
  ST.lift render

{-| Boucle sur les caractères et événements envoyés par l'utilisateur.
-}
handleInput :: ReaderT DecodedCsv (StateT DisplayState Curses) Int
handleInput = do
  the_emojis <- R.lift $ use emojis
  let userReadyOrResize event = or $ (event ==) <$> [
          EventCharacter '\n',
          EventResized
        ]
      n = fromIntegral $ length the_emojis
  s <- iterateWhile (not . flip validChoice n) $ do
    ev      <- handleEvents userReadyOrResize
    pchoice <- R.lift $ use inputString
    if ev == EventResized then R.lift redrawMenu >> return ""
                          else return pchoice
  return $ read s

{-| Affiche le menu d'emoji à l'utilisateur et lui permet de choisir son emoji.
-}
emojiMenu :: DecodedCsv -> IO String
emojiMenu init_emojis = runCurses $ do
  -- Configure NCurses
  setEcho False

  let dprompt = displayPrompt (length init_emojis)
      istate  = DisplayState Nothing "" init_emojis dprompt
  (chosen_id, dstate) <- flip runStateT istate $ flip runReaderT init_emojis $ do
    R.lift redrawMenu
    handleInput
  return $ getEmoji (dstate ^. emojis) (chosen_id-1)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
