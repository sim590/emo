
module Display (
  emojiMenu
) where

import Data.List
import Data.Digits
import Data.Char

import Text.Read

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as ST
import Control.Monad.Loops

import System.Random

import Clip
import Utils

import UI.NCurses

data DisplayConf = DisplayConf {
  emojis     :: DecodedCsv,
  nChoice    :: Int,
  inputTitle :: String
}

liftRST :: Curses a -> ReaderT DisplayConf (StateT String Curses) a
liftRST = R.lift . ST.lift

colOs :: Integer
colOs = 2

ctrlKey :: Char -> Char
ctrlKey c = chr (ord c - 96)

truncatedMsg :: String
truncatedMsg = "// Certains choix ont été tronqués ... //"

maxColCount :: Integer -> [(String, String)] -> Integer
maxColCount w es = w `div` spacedColWidth w es

maxIWidth :: [(String, String)] -> Integer
maxIWidth es = fromIntegral $ length $ digits 10 $ length es

spacedColWidth :: Integer -> [(String, String)] -> Integer
spacedColWidth w es = maxColWidth w es + maxIWidth es + colOs

maxColWidth :: Integer -> [(String, String)] -> Integer
maxColWidth winWmax es = min winWmax (eWmax es)
  where comprule (_, e1) (_, e2) = compare (length e1) (length e2)
        eWmax = fromIntegral . length . snd . maximumBy comprule

maxEntryCount :: Integer -> Integer -> [(String, String)] -> Int
maxEntryCount w h es = fromInteger $ (h-3) * maxColCount w es

{-|
   Affiche les choix d'emojis à l'écran.
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

{-|
   Détermine si le choix est valide.

   * ch_str: (String) le choix.
   * n: (Integer) le nombre de choix totaux.
-}
validChoice :: String -> Integer -> Bool
validChoice ch_str n = case readMaybe ch_str of
  Just c -> 1 >= c || c <= n
  _      -> False

doBackspace :: ReaderT DisplayConf (StateT String Curses) ()
doBackspace = do
  dconf <- ask
  istr   <- R.lift ST.get
  win    <- liftRST defaultWindow
  (y, x) <- liftRST $ getCursor win
  let ititle     = inputTitle dconf
      i          = fromInteger x - length ititle
      x0         = fromIntegral $ length ititle
      (beg, end) = splitAt i istr
      resul      = init beg ++ end
  when (x > x0) $ do
    put resul
    liftRST $ updateWindow win $ do
      moveCursor y x0
      clearLine
      drawString resul
      moveCursor y (x-1)

{-|
   Boucle sur les événements du clavier et effectue les actions appropriées.

   * CTRL+Y: efface l'entrée et copie l'emoji associé au choix si l'entrée est valide.
   * CTRL+R: choix aléatoire d'un emoji et le copie dans la presse-papier.
   * CTRL+L: redessine l'écran.
   * CTRL+B: déplace le curseur vers la gauche.
   * CTRL+F: déplace le curseur vers la droite.
   * CTRL+A: déplace le curseur au début de la ligne.
   * CTRL+E: déplace le curseur à la fin de la ligne.
   * CTRL+D: supprime un caractère sous le curseur.
   * Backspace: efface un caractère.
-}
handleEvents :: (Event -> Bool) -> ReaderT DisplayConf (StateT String Curses) Event
handleEvents p = do
  dconf <- ask
  let x0     = fromIntegral $ length (inputTitle dconf)
      xmax s = x0 + fromIntegral (length s)
  iterateUntil p $ do
    win    <- liftRST defaultWindow
    jev    <- liftRST $ getEvent win Nothing
    (y, x) <- liftRST $ getCursor win
    let updateW    = liftRST . updateWindow win
        clearInput = updateW (moveCursor y x0 >> clearLine) >> put ""
        moveRight  = do
          s <- ST.get
          when (x < xmax s) $ updateW $ moveCursor y (x+1)
        moveLeft = when (x > x0) $ updateW $ moveCursor y (x-1)
    ev  <- case jev of
      Just e@(EventCharacter '\n')           -> return e
      Just e@(EventSpecialKey KeyLeftArrow)  -> moveLeft    >> return e
      Just e@(EventSpecialKey KeyRightArrow) -> moveRight   >> return e
      Just e@(EventSpecialKey KeyBackspace)  -> doBackspace >> return e
      Just e@(EventCharacter c) -> do
        s <- ST.get
        let n = fromIntegral $ nChoice dconf
            clearInputAndCopy i = do
              clearInput
              let emoji = getEmoji (emojis dconf) (i-1)
              liftRST $ liftIO $ copyToClipBoard emoji
              updateW $ drawBottomInfo $ emoji ++ " copié dans le presse-papier..."
        if c == ctrlKey 'y' then when (validChoice s n) $ clearInputAndCopy $ read s
        else if c == ctrlKey 'r' then do
          i <- liftRST $ liftIO $ randomRIO (1, length (emojis dconf) - 1)
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
          put $ s ++ [c]
        return e
      Just e -> return e
      _      -> return $ EventUnknown 0
    liftRST render
    return ev

{-|
   Dessine un message d'information au bas de l'écran. Le curseur est
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

{-|
   Dessine l'invite d'entrée pour l'utilisateur.

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

{-|
   Redessine le menu.

   Cette fonction est normalement appelée lorsque le terminal est redimensionné.
-}
redrawMenu :: ReaderT DisplayConf (StateT String Curses) ()
redrawMenu = do
  dconf <- ask
  win   <- liftRST defaultWindow
  inputstr <- R.lift ST.get
  liftRST $ updateWindow win $ do
    clear
    (h, w) <- windowSize
    let n   = nChoice dconf
        title = inputTitle dconf
        esl = emojis dconf
        mec = maxEntryCount w h esl
        scw = fromIntegral $ spacedColWidth w esl
    if fromIntegral w >= maximum [scw, length title, length truncatedMsg] && h >= 3 then
      writeChoices esl >> drawInputTitle title n mec >> drawString inputstr
    else let w_too_small = "err: Fenêtre trop petite..." in
             when (w >= fromIntegral (length w_too_small)) $ drawString w_too_small
  liftRST render

{-|
   Boucle sur les caractères et événements envoyés par l'utilisateur.
-}
handleInput :: ReaderT DisplayConf (StateT String Curses) Int
handleInput = do
  dconf <- ask
  let userReadyOrResize event = or $ (event ==) <$> [
          EventCharacter '\n',
          EventResized
        ]
      n = fromIntegral $ nChoice dconf
  s <- iterateWhile (not . flip validChoice n) $ do
    ev <- handleEvents userReadyOrResize
    pchoice <- R.lift ST.get
    if ev == EventResized then redrawMenu >> return ""
                          else return pchoice
  return $ read s

{-|
   Affiche le menu d'emoji à l'utilisateur et lui permet de choisir son emoji.
-}
emojiMenu :: DecodedCsv -> IO String
emojiMenu esl = runCurses $ do
  -- Configure NCurses
  setEcho False

  let title = "Choix (valeurs entre 1 et "++ show (length esl) ++ ") ? "
      conf  = DisplayConf esl (length esl) title

  chosen_id <- flip evalStateT "" $ flip runReaderT conf $ do
    redrawMenu
    handleInput
  return $ getEmoji esl (chosen_id-1)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
