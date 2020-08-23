
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Display (
  emojiMenu
) where

import Data.List
import Data.Digits
import Data.Char
import Data.Maybe

import Data.Text (Text, append, pack)
import qualified Data.Text as T

import Data.Text.Read

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

data Prompt = Prompt { _prefix      :: Text
                     , _inputText   :: Text
                     , _idx         :: Int
                     , _history     :: [Text]
                     }

data DisplayState = DisplayState { _helpPad :: Maybe Pad
                                 , _emojis  :: DecodedCsv
                                 , _prompt  :: Prompt
                                 }
makeLenses ''Prompt
makeLenses ''DisplayState

colOs :: Integer
colOs = 2

maxHistorySize :: Int
maxHistorySize = 1000

{-| Fournit le caractère correspondant à la combinaison de touche CTRL et le
   caractère d'entrée.
-}
ctrlKey :: Char -> Char
ctrlKey c = chr (ord c - 96)

{-| Message s'affichant lorsque la liste des choix est tronquée.
-}
truncatedMsg :: Text
truncatedMsg = "// Certains choix ont été tronqués ... //"

displayPrompt :: Int -> Text
displayPrompt len = "Choix (valeurs entre 1 et " `append` pack (show len) `append` ") ? "

{-| Le nombre maximal de colonnes.
-}
maxColCount :: Integer -> DecodedCsv -> Integer
maxColCount w es = w `div` spacedColWidth w es

{-| Largeur maximal d'un index considérant le nombre maximal d'indexes.
-}
maxIWidth :: DecodedCsv -> Integer
maxIWidth es = fromIntegral $ length $ digits 10 $ length es

{-| Largeur d'une colonne en considérant la largeur maximale d'une colonne, la
   largeur maximal de l'index et les espaces blancs pour alignement des indexes.
-}
spacedColWidth :: Integer -> DecodedCsv -> Integer
spacedColWidth w es = maxColWidth w es + maxIWidth es + colOs

{-| La largeur maximale d'une colonne considérant la largeur de chacun des
   emojis.
-}
maxColWidth :: Integer -> DecodedCsv -> Integer
maxColWidth winWmax es = min winWmax (eWmax es)
  where comprule (_, e1) (_, e2) = compare (T.length e1) (T.length e2)
        eWmax = fromIntegral . T.length . snd . maximumBy comprule

{-| Le nombre maximal d'emoji pouvant s'afficher sur l'écran.
-}
maxEntryCount :: Integer -> Integer -> DecodedCsv -> Int
maxEntryCount w h es = fromInteger $ (h-3) * maxColCount w es

{-| Affiche une chaîne de caractère avec un attribut donné.
-}
drawTextWithAttr :: Attribute -> Text -> Update ()
drawTextWithAttr attr t = setAttribute attr True >> drawText t >> setAttribute attr False

{-| Affiche du texte en caractères gras.
-}
drawBoldText :: Text -> Update ()
drawBoldText = drawTextWithAttr AttributeBold

{-| Affiche le texte d'aide, c.-à-d. l'aide-mémoire des touches.
-}
showHelp :: StateT DisplayState Curses ()
showHelp = do
  (h, w) <- ST.lift screenSize
  let biggestWidth = maximum (map T.length keyMapsHelpText)
      pWidth       = min w $ fromIntegral biggestWidth + 4
      pHeight      = min h $ fromIntegral (length keyMapsHelpText) + 3
      padY         = div (h - pHeight) 2
      padX         = div (w - pWidth) 2
  when (fromInteger w > pWidth && fromInteger h > pHeight) $ do
    mOldPad <- use helpPad
    jpad    <- case mOldPad of
      jop@(Just _) -> return jop
      _            -> ST.lift $ Just <$> newPad pHeight pWidth
    helpPad .= jpad
    let osWidth len = div (fromInteger pWidth - len) 2
        centered s  = pack (replicate (osWidth $ T.length s) ' ') `append` s
        header      = centered "Aide-mémoire" `append` "\n"
        hpad        = fromJust jpad
    ST.lift $ updatePad hpad 0 0 padY padX h w $ do
      clear
      drawText "\n"       -- passer première ligne pour la bordure ...
      drawBoldText header
    ST.lift $ updatePad hpad 0 0 padY padX h w $ forM_ keyMapsHelpText $ \ l ->
      drawText $ T.replicate (osWidth biggestWidth) " " `append` l `append` "\n"
    ST.lift $ updatePad hpad 0 0 padY padX h w $ drawBox Nothing Nothing

filterEmojis :: DecodedCsv -> MaybeT (StateT DisplayState Curses) DecodedCsv
filterEmojis all_emojis = do
  next_emojis <- hoist liftIO $ fzf all_emojis
  emojis        .= next_emojis
  prompt.prefix .= displayPrompt (length next_emojis)
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
        drawText $ pack (show (i+1)) `append` pack ") " `append` e
        return $ i + 1
  foldM_ drawIthEmoji 0 $ take (maxEntryCount w h es) es
  (y, _) <- cursorPosition
  moveCursor (y+2) 0

{-| Détermine si le choix est valide.

   * ch_str: (String) le choix.
   * n: (Integer) le nombre de choix totaux.
-}
validChoice :: Text -> Integer -> Bool
validChoice ch_str n = case decimal ch_str of
  Right (c, _) -> 1 >= c || c <= n
  _      -> False

{-| Effectue une suppression d'un caractère à gauche du cruseur lorsque le
   celui-ci se trouve sur la ligne d'entrée usager.
-}
doBackspace :: StateT DisplayState Curses ()
doBackspace = do
  (_, w) <- ST.lift screenSize
  promptp <- use $ prompt.prefix
  istr    <- use $ prompt.inputText
  win     <- ST.lift defaultWindow
  (y, x)  <- ST.lift $ getCursor win
  let i          = fromInteger x - T.length promptp
      x0         = fromIntegral $ T.length promptp
      (beg, end) = T.splitAt i istr
      resul      = T.init beg `append` end
  when (x > x0) $ do
    prompt.inputText .= resul
    ST.lift $ updateWindow win $ do
      moveCursor y x0
      clearLine
      drawString resul
      moveCursor y (x-1)

{-| Ramène le prochain élément moins récent dans l'historique.
-}
goUpInHistory :: StateT DisplayState Curses ()
goUpInHistory = do
  hist <- use $ prompt.history
  unless (null hist) $ do
    i     <- use $ prompt.idx
    s     <- use $ prompt.inputText
    hist' <- if i == 0 then prompt.history <.= s : hist
                       else return hist
    i'    <- prompt.idx <.= min (length hist' - 1) (i+1)
    prompt.inputText .= hist' !! i'

{-| Ramène le prochain élément plus récent dans l'historique.
-}
goDownInHistory :: StateT DisplayState Curses ()
goDownInHistory = do
  hist <- use $ prompt.history
  unless (null hist) $ do
    i    <- use $ prompt.idx
    i'   <- prompt.idx <.= max 0 (i-1)
    if i == 1 then do
      prompt.history     %= tail
      prompt.inputText .= head hist
    else when (i>1) $ prompt.inputText .= hist !! i'

resetPrompt :: StateT DisplayState Curses ()
resetPrompt  = do
  i <- prompt.idx <<.= 0
  when (i > 0) $ prompt.history %= tail

{-| Boucle sur les événements du clavier et effectue les actions appropriées.

   * CTRL+H: affiche un texte d'aide.
   * CTRL+Y: efface l'entrée et copie l'emoji associé au choix si l'entrée est valide.
   * CTRL+I: affiche de l'information sur l'emoji.
   * CTRL+R: choix aléatoire d'un emoji et le copie dans la presse-papier.
   * CTRL+P / flèche haut: remonte l'historique
   * CTRL+N / flèche bas: redescend l'historique
   * CTRL+L: redessine l'écran.
   * CTRL+B / flèche gauche: déplace le curseur vers la gauche.
   * CTRL+F / flèche droite: déplace le curseur vers la droite.
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
    promptp <- use $ prompt.prefix
    let x0     = fromIntegral $ T.length promptp
        xmax t = x0 + fromIntegral (T.length t)
    win    <- ST.lift defaultWindow
    jev    <- ST.lift $ getEvent win Nothing
    (y, x) <- ST.lift $ getCursor win
    the_emojis <- use emojis
    let n = fromIntegral $ length the_emojis
        updateW    = ST.lift . updateWindow win
        emoji i         = getEmoji the_emojis (i-1)
        emojiInfo i     = getEmojiInfo the_emojis (i-1)
        emojiPlusInfo i = emoji i `append` " " `append` "(" `append` emojiInfo i `append` ")"
        clearInput = updateW (moveCursor y x0 >> clearLine) >> prompt.inputText .= ""
        moveRight  = do
          t <- use $ prompt.inputText
          when (x < xmax t) $ updateW $ moveCursor y (x+1)
        moveLeft = when (x > x0) $ updateW $ moveCursor y (x-1)
        goInHistory dir = do
          case dir of
            "up"   -> goUpInHistory
            "down" -> goDownInHistory
            _      -> return ()
          t <- use $ prompt.inputText
          hoist (updateWindow win) drawPrompt
          let i = readDecimal t
          updateW $ drawBottomInfo $ if validChoice t n then emojiPlusInfo i else ""
    ev  <- case jev of
      Just e@(EventCharacter '\n')           -> return e
      Just e@(EventSpecialKey KeyLeftArrow)  -> moveLeft           >> return e
      Just e@(EventSpecialKey KeyRightArrow) -> moveRight          >> return e
      Just e@(EventSpecialKey KeyBackspace)  -> doBackspace        >> return e
      Just e@(EventSpecialKey KeyUpArrow)    -> goInHistory ("up"::String)   >> return e
      Just e@(EventSpecialKey KeyDownArrow)  -> goInHistory ("down"::String) >> return e
      Just e@(EventCharacter c) -> do
        t <- use $ prompt.inputText
        let clearInputAndCopy txt = when (validChoice txt n) $ do
              let i = readDecimal txt
              clearInput
              ST.lift $ liftIO $ copyToClipBoard $ emoji i
              updateW $ drawBottomInfo $ emojiPlusInfo i `append` " copié dans le presse-papier..."
              resetPrompt
              when (validChoice txt n) $ prompt.history %= (\ hist ->
                  txt : (if length hist >= maxHistorySize then init hist else hist)
                )
        if      c == ctrlKey 'y' then clearInputAndCopy t
        else if c == ctrlKey 'h' then showHelp
        else if c == ctrlKey 't' then do
          mfzfed_emojis <- runMaybeT $ filterEmojis all_emojis
          redrawMenu
          when (isNothing mfzfed_emojis) $ updateW $ drawBottomInfo "erreur: fzf est introuvable..."
        else if c == ctrlKey 'i' then when (validChoice t n) $ do
          let i = readDecimal t
          updateW $ drawBottomInfo $ emojiPlusInfo i
        else if c == ctrlKey 'r' then do
          i <- ST.lift $ liftIO $ randomRIO (1, length the_emojis - 1)
          clearInputAndCopy $ pack $ show i
        -- Quelques touches de readline
        else if c == ctrlKey 'p' then goInHistory ("up" :: String)
        else if c == ctrlKey 'n' then goInHistory ("down" :: String)
        else if c == ctrlKey 'l' then redrawMenu
        else if c == ctrlKey 'u' then clearInput
        else if c == ctrlKey 'd' then when (x < xmax t) $ moveRight >> doBackspace
        else if c == ctrlKey 'b' then moveLeft
        else if c == ctrlKey 'f' then moveRight
        else if c == ctrlKey 'a' then updateW $ moveCursor y x0
        else if c == ctrlKey 'e' then updateW $ moveCursor y $ xmax t
        -- On affiche tout autre caractère à la ligne.
        else do
          updateW $ drawString [c]
          prompt.inputString .= s ++ [c]
        return e
      Just e -> return e
      _      -> return $ EventUnknown 0
    ST.lift render
    return ev

{-| Dessine un message d'information au bas de l'écran. Le curseur est
   repositionné à son emplacement de départ.

   Hypothèse: le curseur Curses se trouve au niveau de la ligne d'entrée usager.
-}
drawBottomInfo :: Text -> Update ()
drawBottomInfo infos = do
  (y, x) <- cursorPosition
  (_, w) <- (& _2 %~ fromInteger) <$> windowSize
  moveCursor (y+1) 0
  clearLine
  unless (T.null infos) $ drawText $ pack "`--> " `append` infos
  moveCursor y x

{-| Dessine l'invite d'entrée pour l'utilisateur.

   Hypothèse: le curseur Curses se trouve au niveau de la ligne d'entrée usager.
-}
drawPrompt :: StateT DisplayState Update ()
drawPrompt = do
  (h, w)   <- ST.lift windowSize
  esl      <- use emojis
  promptp  <- use $ prompt.prefix
  inputtxt <- use $ prompt.inputText
  let mec = maxEntryCount w h esl
      n   = length esl
  ST.lift $ do
    (y, _) <- cursorPosition

    moveCursor (y-1) 0
    clearLine
    unless (n <= mec) $ drawString truncatedMsg

    moveCursor y 0
    clearLine
    drawString promptp
    drawString inputstr

{-| Redessine le menu.

   Cette fonction est normalement appelée lorsque le terminal est redimensionné.
-}
redrawMenu :: StateT DisplayState Curses ()
redrawMenu = do
  win      <- ST.lift defaultWindow
  esl      <- use emojis
  promptp  <- use $ prompt.prefix
  (h, w)   <- ST.lift screenSize
  ST.lift $ updateWindow win clear
  let scw = fromIntegral $ spacedColWidth w esl
      sufficient_space = fromIntegral w >= maximum [scw, T.length promptp, T.length truncatedMsg]
                         && h >= 3
  if sufficient_space then do
    ST.lift $ updateWindow win $ writeChoices esl
    hoist (updateWindow win) drawPrompt
  else ST.lift $ updateWindow win $ do
    let w_too_small = "err: Fenêtre trop petite..."
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
    pchoice <- R.lift $ use $ prompt.inputText
    if ev == EventResized then R.lift redrawMenu >> return ""
                          else return pchoice
  return $ readDecimal s

{-| Affiche le menu d'emoji à l'utilisateur et lui permet de choisir son emoji.
-}
emojiMenu :: DecodedCsv -> IO Text
emojiMenu init_emojis = runCurses $ do
  -- Configure NCurses
  setEcho False

  let promptp    = displayPrompt (length init_emojis)
      the_prompt = Prompt promptp "" 0 []
      istate     = DisplayState Nothing init_emojis the_prompt
  (chosen_id, dstate) <- flip runStateT istate $ flip runReaderT init_emojis $ do
    R.lift redrawMenu
    handleInput
  return $ getEmoji (dstate ^. emojis) (chosen_id-1)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
