
module Display (
  maxEntryCount,
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

import Clip

import UI.NCurses

type DecodedCsv = [(String, String)]

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
maxEntryCount w h es = fromInteger $ (h-2) * maxColCount w es

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

{-|
   Boucle sur les événements du clavier et effectue les actions appropriées.

   * CTRL+U: efface l'entrée de l'utilisateur.
   * CTRL+Y: efface l'entrée et copie l'emoji associé au choix si l'entrée est valide.
   * Backspace: efface un caractère.
-}
accAndEchoUntil :: (Event -> Bool) -> ReaderT DisplayConf (StateT String Curses) Event
accAndEchoUntil p = do
  dconf <- ask
  let x0 = fromIntegral $ length (inputTitle dconf)
      clear_chars = [
           ctrlKey 'u',
           ctrlKey 'y'
        ]
  R.lift $ iterateUntil p $ do
        win    <- ST.lift defaultWindow
        jev    <- ST.lift $ getEvent win Nothing
        (y, x) <- ST.lift $ getCursor win
        ev  <- case jev of
          Just e@(EventCharacter '\n') -> return e
          Just e@(EventSpecialKey KeyBackspace) -> do
            ST.lift $ updateWindow win $ when (x > x0) $ do
              moveCursor y (x-1)
              drawString " "
              moveCursor y (x-1)
            s <- ST.get
            put $ if null s then s else init s
            return e
          Just e@(EventCharacter c) -> do
            if or ((== c) <$> clear_chars) then do
              s <- ST.get
              ST.lift $ updateWindow win $ do
                moveCursor y x0
                clearLine
              let n = fromIntegral $ nChoice dconf
              when (c == ctrlKey 'y' && validChoice s n) $ do
                let i = read s
                ST.lift $ liftIO $ copyToClipBoard $ snd $ emojis dconf !! (i-1)
              put ""
            else do
              ST.lift $ updateWindow win $ drawString [c]
              s <- ST.get
              put $ s ++ [c]
            return e
          Just e -> return e
          _      -> return $ EventUnknown 0
        ST.lift render
        return ev

{-|
   Dessine l'invite d'entrée pour l'utilisateur.
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
      refresh = redrawMenu
      n = fromIntegral $ nChoice dconf
  s <- iterateWhile (not . flip validChoice n) $ do
    ev <- accAndEchoUntil userReadyOrResize
    pchoice <- R.lift ST.get
    if ev == EventResized then
      refresh
    else unless (validChoice pchoice n) $ R.lift $ ST.put ""
    return pchoice
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
  return $ snd $ esl !! (chosen_id-1)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
