module Options (
  Options (..),
  defaultOptions,
  showHelp,
  options
) where

import System.Directory
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

newtype Options = Options {
  optInfile :: IO String
}

programVersion :: Double
programVersion = 0.1

defaultOptions :: Options
defaultOptions = Options {
  optInfile = getXdgDirectory XdgConfig "emo.csv"
}

showHelp :: IO ()
showHelp = do
  prg <- getProgName
  hPutStrLn stderr (usageInfo (prg ++ " -- " ++ "choisir son émoticône") options)

options :: [ OptDescr (Options -> IO Options) ]
options =
    [
      Option "v" ["version"]
        (NoArg
            (\ _ -> do
              hPutStrLn stderr $ "Version " ++ show programVersion
              exitSuccess
            )
        )
        "Affiche la version du programme.",
      Option "a" ["aide"]
        (NoArg (\ _ -> showHelp >> exitSuccess))
        "Affiche de l'aide.",
      Option "f" []
        (ReqArg (\ arg opts -> return opts { optInfile = return arg }) "FICHIER")
        "Fichier d'entrée contenant les émoticônes."
    ]

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
