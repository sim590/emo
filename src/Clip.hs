
module Clip (
  copyToClipBoard
) where

import Data.Text
import qualified Data.Text.IO as DIO

import System.IO
import System.Process

{-|
   Copie une chaîne de caractère dans le presse-papier. Cela utilise le programme `xclip` sur X.
-}
copyToClipBoard :: Text -> IO ()
copyToClipBoard choice = do
  (Just hin, _, _, hp) <- createProcess (proc "xclip" ["-selection", "c"]) { std_in = CreatePipe }
  DIO.hPutStr hin choice
  hClose hin
  _ <- waitForProcess hp
  return ()

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

