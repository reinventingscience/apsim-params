{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module APSIM.Decode
  ( decodeVariable
  , decodeVariables
  ) where

import Data.Binary.Get
       ( Get
       , getByteString
       , getDoublehost
       , getInt32le
       , getWord8
       , isEmpty
       , runGet
       )
import qualified Data.ByteString.Lazy as LBS
import Data.Dependent.Sum
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(..))
import Data.Some
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.Calendar (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)

import APSIM.Data

import Debug.Trace
import Formatting

decodeVariable :: Some APSIMType -> LBS.ByteString -> DSum APSIMType Identity
decodeVariable dt = runGet (varDecoder dt)

decodeVariables :: Some APSIMType -> LBS.ByteString -> [DSum APSIMType Identity]
decodeVariables dt = runGet (varListDecoder dt)

varDecoder :: Some APSIMType -> Get (DSum APSIMType Identity)
varDecoder (Some APSIMInteger) =
  getInt32le -- Assumes server machine is little-endian
  <&> fromIntegral
  <&> (APSIMInteger ==>)
varDecoder (Some APSIMDouble) =
  getDoublehost -- Assumes server machine is the same endianness as us, and has doubles the same size
  <&> (APSIMDouble ==>)
varDecoder (Some APSIMBoolean) = do
  byte <- getWord8 -- Assumes server machine is little-endian
  return $ APSIMBoolean ==> (byte /= 0)
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
textDecoder = do
  len <- getInt32le -- Assumes server machine is little-endian
  bs <- getByteString $ fromIntegral len
  return $ T.decodeUtf8 bs -- TODO: handle decoding error

parseDate :: Text -> Day
parseDate txt =
  parseTimeOrError False defaultTimeLocale "%Y-%m-%d" (T.unpack txt)
