
module Fzf (
  fzf
) where

import Data.Either

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
      lift $ BL.hPut hin $ encode csv
      lift $ hClose hin
      _ <- lift $ waitForProcess hp
      ecsv <- lift $ decode <$> hGetContents hout
      guard $ isRight ecsv
      return $ fromRight [] ecsv

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

