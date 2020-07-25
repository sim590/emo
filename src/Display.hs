
module Display (
  maxEntryCount,
  emojiMenu
) where

import Data.List
import Data.Digits
import Data.Char
import Data.Maybe

import Text.Read

import Control.Monad
import Control.Monad.Loops


import UI.NCurses

type DecodedCsv = [(String, String)]

colOs :: Integer
colOs = 2


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

accAndEchoUntil :: Window -> Integer -> (Event -> Bool) -> Curses [Event]
accAndEchoUntil w x0 p = fmap catMaybes $ unfoldWhileM (\ (Just ev) -> not $ p ev) $ do
  jev <- getEvent w Nothing
  updateWindow w $ case jev of
    Just EventResized                   -> return ()
    Just (EventCharacter '\n')          -> return ()
    Just (EventSpecialKey KeyBackspace) -> do
      (y, x) <- cursorPosition
      when (x > x0) $ do
        moveCursor y (x-1)
        drawString " "
        moveCursor y (x-1)
    Just (EventCharacter c) ->
      if c ==  chr (ord 'u' - 96) then do
        (y, _) <- cursorPosition
        moveCursor y x0
        clearLine
      else
        drawString [c]
    _                                   -> return ()
  render
  return jev

drawInputTitle :: String -> Int -> Int -> Update ()
drawInputTitle title nChoice mec = do
  (y, _) <- cursorPosition

  moveCursor (y-1) 0
  clearLine
  unless (nChoice <= mec) $ drawString truncatedMsg

  moveCursor y 0
  clearLine
  drawString title

updateMenuHandler :: DecodedCsv -> String -> Update ()
updateMenuHandler esl title = do
  clear
  (h, w) <- windowSize
  let nChoice = length esl
      mec     = maxEntryCount w h esl
      scw     = fromIntegral $ spacedColWidth w esl
  if fromIntegral w >= maximum [scw, length title, length truncatedMsg] && h >= 3 then
    writeChoices esl >> drawInputTitle title nChoice mec
  else let w_too_small = "err: Fenêtre trop petite..." in
           when (w >= fromIntegral (length w_too_small)) $ drawString w_too_small

handleInput :: Window -> String -> DecodedCsv -> Curses Int
handleInput win title esl = do
  let validChoice mchoice = case readMaybe mchoice of
        Just c -> 1 >= c || c <= nChoice
        _      -> False
      nChoice = length esl
  s <- iterateWhile (not . validChoice) $ do
    updateWindow win $ updateMenuHandler esl title
    render
    (_, x0) <- getCursor win
    let refresh_needed event = or $ (event ==) <$> [
            EventCharacter '\n',
            EventResized
          ]
    evs <- accAndEchoUntil win x0 refresh_needed
    let evstring            = map (\ (EventCharacter c) -> c) cevs
        cevs                = filter isDigitOrQuitCmd evs
        isDigitOrQuitCmd ev =
          case ev of
            EventCharacter d -> isDigit d
            _                -> False
    return evstring
  return (read s)

emojiMenu :: DecodedCsv -> IO String
emojiMenu esl = runCurses $ do
  -- Configure NCurses
  setEcho False
  win <- defaultWindow

  -- Affiche les choix à l'écran
  let title = "Choix (valeurs entre 1 et "++ show (length esl) ++ ") ? "
  updateWindow win $ do
    moveCursor 0 0
    updateMenuHandler esl title
  render

  chosen_id <- handleInput win title esl
  return $ snd $ esl !! (chosen_id-1)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
