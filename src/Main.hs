
module Main where

import Data.Char
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.UTF8 as BLU

import System.Exit
import System.Environment
import System.Console.GetOpt
import System.Posix.Signals

import Control.Monad

import qualified Options as Opts
import Display
import Clip

main :: IO ()
main =
     installHandler sigTERM Default Nothing
  >> installHandler sigINT  Default Nothing
  >> installHandler sigHUP  Default Nothing
  >> do

  --  Analyse des arguments
  args <- getArgs
  let (actions, _, errors) = getOpt RequireOrder Opts.options args
  opts <- foldl (>>=) (return Opts.defaultOptions) actions
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

  -- Choix de l'emoji par le menu
  let emojis = V.toList emojisVector
  choice <- emojiMenu emojis

  copyToClipBoard choice

  exitSuccess

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
