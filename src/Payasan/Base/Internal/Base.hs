{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Base
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Base
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Base
  ( 

    Decimal
  , Seconds
  , Onset 
  , BPM
  , TimeSig
  , barLength
  , beatLength
  , bpmFromQnl

  , TimeSpan(..)
  , unionTimeSpan

  , Beat
  , beatToSeconds
  , secondsToBeat
  , intToBeat

  , PitchSet
  , pitchSet
  , getPitches

  , replaceAns

  )  where


import Data.Data
import Data.Fixed
import Data.Monoid
import qualified Data.Set as Set

type Decimal = Fixed E9


type Seconds = Decimal
type Onset   = Seconds


type BPM     = Decimal


type TimeSig  = (Int,Int)
  



barLength :: TimeSig -> BPM -> Seconds
barLength (n,d) bpm = fromIntegral n * (dsclf * qnlen)
  where
    qnlen = 60 / bpm
    dsclf = 4 / fromIntegral d      -- denominator scaling factor


beatLength :: TimeSig -> BPM -> Seconds 
beatLength ts@(n,_) bpm = 
    let tot = barLength ts bpm in tot / fromIntegral n


bpmFromQnl :: Seconds -> BPM 
bpmFromQnl qnl = 60 / qnl

--------------------------------------------------------------------------------
-- Time spans

-- | Time span - note does not support monoid instance.
--
data TimeSpan = TimeSpan 
    { tspan_start       :: !Seconds
    , tspan_duration    :: !Seconds
    } 
  deriving (Data,Eq,Show,Typeable)


unionTimeSpan :: TimeSpan -> TimeSpan -> TimeSpan
unionTimeSpan (TimeSpan st1 d1) (TimeSpan st2 d2) = 
    let st  = min st1 st2
        end = max (st1 + d1) (st2 + d2)
    in TimeSpan st (end - st)


--------------------------------------------------------------------------------
-- Beat

-- | 1.0 = 1 qn (2.0 = hn, 4.0 = wn, 0.5 = en)
--
newtype Beat = Beat { getBeat :: Fixed E9 }
  deriving (Data,Eq,Ord,Num,Fractional,Real,RealFrac,Typeable)  

instance Show Beat where show = show . getBeat


beatToSeconds :: BPM -> Beat -> Seconds
beatToSeconds bpm a = let dwn_secs = (realToFrac $ 60 / bpm)
                      in realToFrac a * dwn_secs

secondsToBeat :: BPM -> Seconds -> Beat
secondsToBeat bpm d = let dwn_secs = (realToFrac $ 60 / bpm)
                      in realToFrac d / dwn_secs

-- | Use for interpreting time signatures, etc.
--
-- 1 = wn, 2 = hn, 4 = qn, 8 = en ...
-- 
intToBeat :: Int -> Beat
intToBeat i = 4.0 / fromIntegral i



--------------------------------------------------------------------------------
-- Pitch Set

newtype PitchSet pch = PitchSet { getPitchSet :: Set.Set pch }

instance Ord pch => Monoid (PitchSet pch)where
  mempty = PitchSet $ mempty
  a `mappend` b = PitchSet $ getPitchSet a `mappend` getPitchSet b


pitchSet :: Ord pch => [pch] -> PitchSet pch
pitchSet = PitchSet . Set.fromList

getPitches :: Ord pch => PitchSet pch -> [pch]
getPitches = Set.toAscList . getPitchSet




--------------------------------------------------------------------------------

replaceAns :: Functor m => a -> m z -> m a
replaceAns a = fmap (const a)

