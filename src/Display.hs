
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
import qualified Control.Monad.State as ST
import Control.Monad.Loops


import UI.NCurses

type DecodedCsv = [(String, String)]

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

accAndEchoUntil :: Integer -> (Event -> Bool) -> StateT String Curses ()
accAndEchoUntil x0 p = void go
  where
        clear_chars = [
             ctrlKey 'u'
          ]
        go = iterateUntil p $ do
          win <- ST.lift defaultWindow
          jev <- ST.lift $ getEvent win Nothing
          ev  <- case jev of
            Just (EventCharacter '\n') -> return $ EventCharacter '\n'
            Just (EventSpecialKey KeyBackspace) -> do
              ST.lift $ updateWindow win $ do
                (y, x) <- cursorPosition
                when (x > x0) $ do
                  moveCursor y (x-1)
                  drawString " "
                  moveCursor y (x-1)
              s <- ST.get
              put $ if null s then s else init s
              return $ EventSpecialKey KeyBackspace
            Just (EventCharacter c) -> do
              if or ((== c) <$> clear_chars) then do
                s <- ST.get
                ST.lift $ updateWindow win $ do
                  (y, _) <- cursorPosition
                  moveCursor y x0
                  clearLine
                put ""
              else do
                ST.lift $ updateWindow win $ drawString [c]
                s <- ST.get
                put $ s ++ [c]
              return $ EventCharacter c
            Just e -> return e
            _      -> return $ EventUnknown 0
          ST.lift render
          return ev

drawInputTitle :: String -> Int -> Int -> Update ()
drawInputTitle title nChoice mec = do
  (y, _) <- cursorPosition

  moveCursor (y-1) 0
  clearLine
  unless (nChoice <= mec) $ drawString truncatedMsg

  moveCursor y 0
  clearLine
  drawString title

updateMenuHandler :: DecodedCsv -> String -> String -> Update ()
updateMenuHandler esl title inputstr = do
  clear
  (h, w) <- windowSize
  let nChoice = length esl
      mec     = maxEntryCount w h esl
      scw     = fromIntegral $ spacedColWidth w esl
  if fromIntegral w >= maximum [scw, length title, length truncatedMsg] && h >= 3 then
    writeChoices esl >> drawInputTitle title nChoice mec >> drawString inputstr
  else let w_too_small = "err: Fenêtre trop petite..." in
           when (w >= fromIntegral (length w_too_small)) $ drawString w_too_small

{-|
   Boucle sur les caractères et événements envoyés par l'utilisateur.
-}
handleInput :: String -> DecodedCsv -> StateT String Curses Int
handleInput title esl = do
  let validChoice mchoice = case readMaybe mchoice of
        Just c -> 1 >= c || c <= nChoice
        _      -> False
      nChoice = length esl
      refresh_needed event = or $ (event ==) <$> [
            EventCharacter '\n',
            EventResized
          ]
      refresh :: Window -> StateT String Curses ()
      refresh win = do
        pchoice <- ST.get
        ST.lift $ updateWindow win $ updateMenuHandler esl title pchoice
        ST.lift render
  s <- iterateWhile (not . validChoice) $ do
    win     <- ST.lift defaultWindow
    (_, x0) <- ST.lift $ getCursor win
    accAndEchoUntil x0 refresh_needed
    pchoice <- ST.get
    unless (validChoice pchoice) $ ST.put ""
    refresh win
    return pchoice
  return $ read s

emojiMenu :: DecodedCsv -> IO String
emojiMenu esl = runCurses $ do
  -- Configure NCurses
  setEcho False
  win <- defaultWindow

  -- Affiche les choix à l'écran
  let title = "Choix (valeurs entre 1 et "++ show (length esl) ++ ") ? "
  updateWindow win $ do
    moveCursor 0 0
    updateMenuHandler esl title ""
  render

  chosen_id <- flip evalStateT "" $ handleInput title esl
  return $ snd $ esl !! (chosen_id-1)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
