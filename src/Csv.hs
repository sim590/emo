
module Csv (
  decode,
  encode,
  DecodedCsv
) where

import Data.Char
import Data.Csv ( defaultDecodeOptions
                , decDelimiter
                , defaultEncodeOptions
                , encDelimiter
                )
import qualified Data.Csv as C

import qualified Data.Vector as V

import Data.ByteString.Lazy.UTF8 (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as BLU

{-|
   Format d'un fichier CSV d'emo décodé.
-}
type DecodedCsv = [(String, String)]

{-| Délimiteur des champs du fichier CSV.
-}
delimiter :: Int
delimiter =  ord ':'

decode :: String -> Either String DecodedCsv
decode s = V.toList <$> C.decodeWith myDecodeOptions C.NoHeader (BLU.fromString s)
  where myDecodeOptions = defaultDecodeOptions { decDelimiter = fromIntegral delimiter }

encode :: DecodedCsv -> ByteString
encode = C.encodeWith myEncodeOptions
  where myEncodeOptions = defaultEncodeOptions { encDelimiter = fromIntegral delimiter }

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

