
module Main where

import Data.Maybe

import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt
import System.Posix.Signals
import System.Random

import Control.Monad

import qualified Options as Opts
import qualified Csv
import Display
import Clip
import Utils

{-|
   Sélectionne un emoji selon les options passés à la ligne de commande.

   * Si l'option -`c` a été passée la ligne de commande, alors le choix récupéré
     directement.
   * Sinon, le menu interactif est affiché à l'utilisateur.
-}
selectEmoji :: Opts.Options -> Csv.DecodedCsv -> IO String
selectEmoji opts emojis = do
  let cmdline_choice = fromJust $ Opts.optChoice opts
  case Opts.optSelect opts of
    Opts.CmdLine -> do
      when (length emojis <= cmdline_choice || cmdline_choice <= 0) $ do
        hPutStrLn stderr Opts.optChoiceOutOfRangeErrMsg
        exitFailure
      return $ getEmoji emojis cmdline_choice
    Opts.Menu -> emojiMenu emojis

{-|
   Le point d'entrée du programme.
-}
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
  inFile <- Opts.optInfile opts
  csv    <- readFile inFile
  emojis <- case Csv.decode csv of
              Left err -> do
                putStrLn err
                exitFailure
              Right es' -> return es'

  emoji <- if Opts.optRandom opts then getEmoji emojis <$> randomRIO (1, length emojis - 1)
                             else selectEmoji opts emojis
  copyToClipBoard emoji
  unless (Opts.optSilent opts) $ putStrLn emoji

  exitSuccess

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
