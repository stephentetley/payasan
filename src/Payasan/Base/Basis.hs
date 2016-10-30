{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Basis
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Basis (or Prelude...)
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Basis
  ( 

    Decimal
  , Seconds
  , BPM

  , PitchOrd(..)

  , MidiPitch

  , Meter(..)
  , Time(..)

  )  where




import Data.Data
import Data.Fixed



type Decimal = Fixed E9


type Seconds = Decimal

type BPM     = Decimal



class PitchOrd a where
  equivalent :: a -> a -> Bool
  isHigher   :: a -> a -> Bool
  isLower    :: a -> a -> Bool



newtype MidiPitch = MidiPitch Int
  deriving (Enum,Eq,Ord,Num,Real,Integral,Show)




-- | CommonTime = 4/4
--   CutTime = 2/4
--
-- TODO - add free metered.
--
data Meter = Unmetered | TimeSig Time
  deriving (Data,Eq,Ord,Show,Typeable)


data Time = Time Int Int
  deriving (Data,Eq,Ord,Show,Typeable)
