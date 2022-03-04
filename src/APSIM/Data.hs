{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module APSIM.Data
  ( APSIMType(..)
  , Variable(..)
  , fAPSIMValue
  , Some(..)
  ) where

import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum
import Data.Functor.Identity (Identity(..))
import Data.GADT.Compare
import Data.GADT.Show
import Data.Some
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Type.Equality
import Generics.Deriving ()

import Formatting
import Formatting.Time

data APSIMType a where
  APSIMInteger :: APSIMType Int
  APSIMDouble  :: APSIMType Double
  APSIMBoolean :: APSIMType Bool
  APSIMDate    :: APSIMType Day
  APSIMString  :: APSIMType Text

deriveArgDict ''APSIMType

-- Boilerplate needed since we can't derive Generic for a GADT

instance Eq (APSIMType a) where
  (==) = defaultEq

instance GEq APSIMType where
  geq APSIMInteger APSIMInteger = Just Refl
  geq APSIMDouble  APSIMDouble  = Just Refl
  geq APSIMBoolean APSIMBoolean = Just Refl
  geq APSIMDate    APSIMDate    = Just Refl
  geq APSIMString  APSIMString  = Just Refl
  geq _            _            = Nothing

instance GCompare APSIMType where
  gcompare APSIMInteger APSIMInteger = GEQ
  gcompare APSIMInteger _            = GLT
  gcompare _            APSIMInteger = GGT

  gcompare APSIMDouble  APSIMDouble  = GEQ
  gcompare APSIMDouble  _            = GLT
  gcompare _            APSIMDouble  = GGT

  gcompare APSIMBoolean APSIMBoolean = GEQ
  gcompare APSIMBoolean _            = GLT
  gcompare _            APSIMBoolean = GGT

  gcompare APSIMDate    APSIMDate    = GEQ
  gcompare APSIMDate    _            = GLT
  gcompare _            APSIMDate    = GGT

  gcompare APSIMString  APSIMString  = GEQ

instance Show (APSIMType a) where
  showsPrec _ APSIMInteger = showString "APSIMInteger"
  showsPrec _ APSIMDouble  = showString "APSIMDouble"
  showsPrec _ APSIMBoolean = showString "APSIMBoolean"
  showsPrec _ APSIMDate    = showString "APSIMDate"
  showsPrec _ APSIMString  = showString "APSIMString"

instance GShow APSIMType where
  gshowsPrec = showsPrec

data Variable = Variable
  { vAddress :: !Text -- E.g. "Clock.Today"
  , vType    :: !(Some APSIMType)
  } deriving (Eq, Ord, Show)

fAPSIMValue :: Format r (DSum APSIMType Identity -> r)
fAPSIMValue = later $ \case
  (APSIMInteger :=> Identity d) -> bprint int d
  (APSIMDouble :=> Identity d) -> bprint float d
  (APSIMBoolean :=> Identity d) -> bprint build d
  (APSIMDate :=> Identity d) -> bprint dateDash d
  (APSIMString :=> Identity t) -> bprint stext t
