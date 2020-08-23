
module Csv (
  decode,
  encode,
  DecodedCsv
) where

import Data.Char
import Data.Text
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Lazy.Encoding
import Data.Csv ( defaultDecodeOptions
                , decDelimiter
                , defaultEncodeOptions
                , encDelimiter
                )
import qualified Data.Csv as C

import qualified Data.Vector as V

{-|
   Format d'un fichier CSV d'emo décodé.
-}
type DecodedCsv = [(Text, Text)]

{-| Délimiteur des champs du fichier CSV.
-}
delimiter :: Int
delimiter = ord ':'

decode :: Text -> Either String DecodedCsv
decode t = V.toList <$> C.decodeWith myDecodeOptions C.NoHeader (encodeUtf8 $ fromStrict t)
  where myDecodeOptions = defaultDecodeOptions { decDelimiter = fromIntegral delimiter }

encode :: DecodedCsv -> Text
encode = toStrict . decodeUtf8 . C.encodeWith myEncodeOptions
  where myEncodeOptions = defaultEncodeOptions { encDelimiter = fromIntegral delimiter }

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

