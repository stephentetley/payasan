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

  , Meter(..)
  , TimeRatio(..)

  , barLength
  , quarterNoteLength

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




-- | CommonTime = 4/4
--   CutTime = 2/4
--
data Meter = Unmetered | TimeSig TimeRatio
  deriving (Data,Eq,Ord,Show,Typeable)


data TimeRatio = TimeRatio Int Int
  deriving (Data,Eq,Ord,Show,Typeable)


-- note use length in naming to imply Seconds...

barLength :: BPM -> TimeRatio -> Seconds
barLength bpm (TimeRatio n d) =
    (realToFrac n / realToFrac d) * (4 * quarterNoteLength bpm)


quarterNoteLength :: BPM -> Seconds
quarterNoteLength bpm = realToFrac $ 60 / bpm
