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
  , TimeSig(..)

  , barLength
  , quarterNoteLength

  )  where



-- Basis should have no dependencies on other Payasan modules


import Data.Data
import Data.Fixed



type Decimal = Fixed E9


type Seconds = Decimal

type BPM     = Decimal


-- | Pitches can be spelled differently but still represent the
-- same absolute pitch (frequency), hence Haskell's Ord class
-- does not really apply 
class PitchOrd a where
  equivalent :: a -> a -> Bool
  isHigher   :: a -> a -> Bool
  isLower    :: a -> a -> Bool




-- TODO - this isn't strictly metering as meter needs more 
-- information that just Time Signature
data Meter = Unmetered | Metered TimeSig
  deriving (Data,Eq,Ord,Show,Typeable)

  
  
-- | CommonTime = 4/4
--   CutTime = 2/4
--
data TimeSig = TimeSig Int Int
  deriving (Data,Eq,Ord,Show,Typeable)


-- note use length in naming to imply Seconds...

barLength :: BPM -> TimeSig -> Seconds
barLength bpm (TimeSig n d) =
    (realToFrac n / realToFrac d) * (4 * quarterNoteLength bpm)


quarterNoteLength :: BPM -> Seconds
quarterNoteLength bpm = realToFrac $ 60 / bpm
