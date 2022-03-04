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
  APSIMDouble :: APSIMType Double
  APSIMString :: APSIMType Text
  APSIMDate :: APSIMType Day

deriveArgDict ''APSIMType

-- Boilerplate needed since we can't derive Generic for a GADT

instance Eq (APSIMType a) where
  (==) = defaultEq

instance GEq APSIMType where
  geq APSIMDouble APSIMDouble = Just Refl
  geq APSIMString APSIMString = Just Refl
  geq APSIMDate APSIMDate     = Just Refl
  geq _ _                     = Nothing

instance GCompare APSIMType where
  gcompare APSIMDouble APSIMDouble = GEQ
  gcompare APSIMDouble _           = GLT
  gcompare _ APSIMDouble           = GGT

  gcompare APSIMString APSIMString = GEQ
  gcompare APSIMString _           = GLT
  gcompare _ APSIMString           = GGT

  gcompare APSIMDate APSIMDate     = GEQ

instance Show (APSIMType a) where
  showsPrec _ APSIMDouble = showString "APSIMDouble"
  showsPrec _ APSIMString = showString "APSIMString"
  showsPrec _ APSIMDate   = showString "APSIMDate"

instance GShow APSIMType where
  gshowsPrec = showsPrec

data Variable = Variable
  { vAddress :: !Text -- E.g. "[Clock].Today"
  , vType    :: !(Some APSIMType)
  } deriving (Eq, Ord, Show)

fAPSIMValue :: Format r (DSum APSIMType Identity -> r)
fAPSIMValue = later $ \case
  (APSIMDouble :=> Identity d) -> bprint float d
  (APSIMString :=> Identity t) -> bprint stext t
  (APSIMDate :=> Identity d) -> bprint dateDash d
