
module Display (
  maxEntryCount,
  writeChoices,
  emojiMenu
) where

import Data.List
import Data.Digits
import Data.Char
import Data.Maybe

import Control.Monad
import Control.Monad.Loops


import UI.NCurses

type DecodedCsv = [(String, String)]

colOs :: Integer
colOs = 2

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

writeChoices :: [(String, String)] -> Window -> Curses ()
writeChoices es win = updateWindow win $ do
  (h, w) <- windowSize
  let mcc = maxColCount w es
      scw = spacedColWidth w es
      drawIthEmoji i (_, e) = do
        let (y, x) = (i `div` mcc, (i `mod` mcc) * (scw + colOs))
        moveCursor y x
        drawString $ show (i+1) ++ ") " ++ e
        return $ i + 1
  foldM_ drawIthEmoji 0 $ take (maxEntryCount w h es) es

waitFor :: Window -> (Event -> Bool) -> Curses Event
waitFor w p = fromJust <$> iterateWhile (\ (Just ev) -> not $ p ev) (getEvent w Nothing)

accAndEchoUntil :: Window -> (Event -> Bool) -> Curses [Event]
accAndEchoUntil w p = fmap catMaybes $ unfoldWhileM (\ (Just ev) -> not $ p ev) $ do
  jev <- getEvent w Nothing
  case jev of
    Just (EventCharacter '\n') -> return ()
    Just (EventCharacter c) -> updateWindow w (drawString [c]) >> render
    _ -> return ()
  return jev

emojiMenu :: DecodedCsv -> IO String
emojiMenu esl = runCurses $ do
  -- Configure NCurses
  setEcho False
  win    <- defaultWindow
  (h, w) <- screenSize

  -- Affiche les choix à l'écran
  let nChoice = length esl
      mec     = maxEntryCount w h esl
  updateWindow win $ moveCursor 0 0
  writeChoices esl win
  updateWindow win $ do
    (y, _) <- cursorPosition
    moveCursor (y+1) 0
    unless (nChoice <= mec) $ drawString "// Certains choix ont été tronqués ... //"
    moveCursor (y+2) 0
    drawString $ "Choix (valeurs entre 1 et "++ show nChoice ++ ") ? "
  render

  -- On attend un choix valide.
  choice <- iterateWhile (\ choice -> let c = read choice in 1 > c || c > nChoice) $ do
    evs <- accAndEchoUntil win (== EventCharacter '\n')
    let evstring = map (\ (EventCharacter c) -> c) cevs
        cevs = filter isDigitOrQuitCmd evs
        isDigitOrQuitCmd ev =
          case ev of
            EventCharacter d -> isDigit d
            _                -> False

    return evstring
  let n = read choice
  return $ snd $ esl !! (n-1)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
