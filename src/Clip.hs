
module Clip (
  copyToClipBoard
) where

import System.IO
import System.Process

copyToClipBoard :: String -> IO ()
copyToClipBoard choice = do
  (Just hin, _, _, hp) <- createProcess (proc "xclip" ["-selection", "c"]) { std_in = CreatePipe }
  hPutStr hin choice
  hClose hin
  _ <- waitForProcess hp
  return ()

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

