
module Main where

import Data.Maybe
import Data.Char
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.UTF8 as BLU

import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt
import System.Posix.Signals
import System.Random

import Control.Monad

import qualified Options as Opts
import Display
import Clip
import Utils

selectEmoji :: Opts.Options -> DecodedCsv -> IO String
selectEmoji opts emojis = do
  let cmdline_choice = fromJust $ Opts.optChoice opts
  case Opts.optSelect opts of
    Opts.CmdLine -> do
      when (length emojis <= cmdline_choice || cmdline_choice <= 0) $ do
        hPutStrLn stderr Opts.optChoiceOutOfRangeErrMsg
        exitFailure
      return $ snd $ emojis !! cmdline_choice
    Opts.Menu -> emojiMenu emojis

main :: IO ()
main =
     installHandler sigTERM Default Nothing
  >> installHandler sigINT  Default Nothing
  >> installHandler sigHUP  Default Nothing
  >> do

  --  Analyse des arguments
  args <- getArgs
  let (actions, _, errors) = getOpt RequireOrder Opts.options args
  opts <- foldl (>>=) (return Opts.defaultOptions) actions >>= Opts.validateOptions
  unless (null errors) $ mapM_ putStr errors >> Opts.showHelp >> exitFailure

  -- Décodage du fichier CSV
  let myDecodeOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord ':') }
  inFile <- Opts.optInfile opts
  csv    <- readFile inFile
  emojisVector <- case decodeWith myDecodeOptions NoHeader (BLU.fromString csv) of
    Left err -> do
      putStrLn err
      exitFailure
    Right es' -> return es'

  let emojis = V.toList emojisVector
  emoji <- if Opts.optRandom opts then snd . (emojis !!) <$> randomRIO (1, length emojis - 1)
                             else selectEmoji opts emojis
  copyToClipBoard emoji
  unless (Opts.optSilent opts) $ putStrLn emoji

  exitSuccess

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
