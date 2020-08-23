
module Fzf (
  fzf
) where

import Data.Either
import Data.Text.Lazy.Encoding
import Data.Text.Lazy (fromStrict)
import qualified Data.Text.IO as DIO

import System.IO
import System.Process
import System.Exit

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

import qualified Data.ByteString.Lazy as BL

import Csv

fzf :: DecodedCsv -> MaybeT IO DecodedCsv
fzf csv = do
  c <- lift $ system "command -v fzf"
  case c of
    ExitFailure _ -> mzero
    _             -> do
      (Just hin, Just hout, _, hp) <- lift $ createProcess (proc "fzf" ["-m"]) { std_in=CreatePipe
                                                                               , std_out=CreatePipe
                                                                               }
      lift $ BL.hPut hin $ encodeUtf8 $ fromStrict $ encode csv
      lift $ hClose hin
      _ <- lift $ waitForProcess hp
      ecsv <- lift $ decode <$> DIO.hGetContents hout
      guard $ isRight ecsv
      return $ fromRight [] ecsv

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

