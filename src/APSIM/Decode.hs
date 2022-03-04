{-# LANGUAGE GADTs #-}
module APSIM.Decode
  ( decodeVariable
  , decodeVariables
  ) where

import Data.Binary.Get
       (Get, getDoublele, getLazyByteStringNul, getWord32le, isEmpty, runGet)
import qualified Data.ByteString.Lazy as LBS
import Data.Dependent.Sum
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(..))
import Data.Some
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)

import APSIM.Data

decodeVariable :: Some APSIMType -> LBS.ByteString -> DSum APSIMType Identity
decodeVariable dt = runGet (varDecoder dt)

decodeVariables :: Some APSIMType -> LBS.ByteString -> [DSum APSIMType Identity]
decodeVariables dt = runGet (varListDecoder dt)

varDecoder :: Some APSIMType -> Get (DSum APSIMType Identity)
varDecoder (Some APSIMDouble) =
  getDoublele -- Assumes server machine is little-endian
  <&> (APSIMDouble ==>)
varDecoder (Some APSIMString) =
  textDecoder
  <&> (APSIMString ==>)
varDecoder (Some APSIMDate) =
  textDecoder
  <&> parseDate
  <&> (APSIMDate ==>)

varListDecoder :: Some APSIMType -> Get [DSum APSIMType Identity]
varListDecoder dt = do
  empty <- isEmpty
  if empty
    then return []
    else do
      val <- varDecoder dt
      vals <- varListDecoder dt
      return (val:vals)

textDecoder :: Get Text
textDecoder = getLazyByteStringNul <&> TL.decodeUtf8 <&> TL.toStrict

parseDate :: Text -> Day
parseDate txt =
  parseTimeOrError False defaultTimeLocale "%Y-%m-%d" (T.unpack txt)
