{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module APSIM.Decode
  ( decodeVariable
  , decodeVariables
  , getInt
  , getDouble
  , getBool
  , getText
  , getDate
  ) where

import Data.Binary.Get
       ( Get
       , getByteString
       , getDoublele
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
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)

import APSIM.Data

import Debug.Trace
import Formatting

decodeVariable :: Some APSIMType -> LBS.ByteString -> DSum APSIMType Identity
decodeVariable dt = runGet (getVar dt)

decodeVariables :: Some APSIMType -> LBS.ByteString -> [DSum APSIMType Identity]
decodeVariables dt = runGet (getVarList dt)

getInt :: Get Int
getInt = getInt32le <&> fromIntegral

getDouble :: Get Double
getDouble = getDoublele

getBool :: Get Bool
getBool = getWord8 <&> (/= 0)

getText :: Get Text
getText = do
  len <- getInt32le
  bs <- getByteString $ fromIntegral len
  return $ T.decodeUtf8 bs -- TODO: handle decoding error

getDate :: Get Day
getDate = getInt <&> parseDate

getVar :: Some APSIMType -> Get (DSum APSIMType Identity)
getVar (Some APSIMInteger) = getInt    <&> (APSIMInteger ==>)
getVar (Some APSIMDouble)  = getDouble <&> (APSIMDouble  ==>)
getVar (Some APSIMBoolean) = getBool   <&> (APSIMBoolean ==>)
getVar (Some APSIMString)  = getText   <&> (APSIMString  ==>)
getVar (Some APSIMDate)    = getDate   <&> (APSIMDate    ==>)

getVarList :: Some APSIMType -> Get [DSum APSIMType Identity]
getVarList dt = do
  empty <- isEmpty
  if empty
    then return []
    else do
      val <- getVar dt
      vals <- getVarList dt
      return (val:vals)

parseDate :: Int -> Day
parseDate i =
  let y = fromIntegral i `div` 10000
      m = i `mod` 10000 `div` 100
      d = i `mod` 100
  in fromGregorian y m d
