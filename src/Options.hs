
module Options (
  Options (..),
  SelectMode (..),
  validateOptions,
  optChoiceOutOfRangeErrMsg,
  defaultOptions,
  showHelp,
  options
) where

import System.Directory
import System.IO
import System.Exit
import System.Environment
import System.Console.GetOpt

import Text.Read

data SelectMode = Menu | CmdLine

data Options = Options {
  optInfile :: IO String,
  optSelect :: SelectMode,
  optChoice :: Maybe Int,
  optSilent :: Bool,
  optRandom :: Bool
}

validateOptions :: Options -> IO Options
validateOptions opts = do
  oc <- case optChoice opts of
    Just c -> return c
    _      -> hPutStrLn stderr optChoiceNotIntErrMsg >> exitFailure
  return opts { optChoice = return oc }

optChoiceOutOfRangeErrMsg :: String
optChoiceOutOfRangeErrMsg = "erreur: le nombre passé à -n est inadmissible."

optChoiceNotIntErrMsg :: String
optChoiceNotIntErrMsg = "erreur: un entier doit être passé à -n."

programVersion :: Double
programVersion = 0.1

defaultOptions :: Options
defaultOptions = Options {
  optInfile = getXdgDirectory XdgConfig "emo.csv",
  optSelect = Menu,
  optChoice = return 0,
  optSilent = False,
  optRandom = False
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
      Option "c" []
        (ReqArg (\ arg opts ->
          return opts { optChoice = readMaybe arg, optSelect = CmdLine }) "NUMÉRO")
        "Le numéro de l'émoticône à choisir (démarre à 1). Lorsque cette option\n\
        \est utilisée, le programme sélectionne directement l'émoticône pour\n\
        \l'utilisateur et quitte.",
      Option "s" []
        (NoArg (\ opts -> return opts { optSilent = True }))
        "Mode silencieux. Ne fonctionne qu'en mode ligne de commande (entre\n\
        \autres avec -c).",
      Option "h" []
        (NoArg (\ opts -> return opts { optRandom = True, optSelect = CmdLine }))
        "Choix de l'émoticône au hasard.",
      Option "f" []
        (ReqArg (\ arg opts -> return opts { optInfile = return arg }) "FICHIER")
        "Fichier d'entrée contenant les émoticônes."
    ]

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
