{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.AltPitch
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Low level pitch representations for backend output.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.AltPitch
  ( 


    MidiPitch(..)
  , CpsPitch(..)
  , HzPitch(..)

  , cps_middle_c
  , toCpsPitch
  
  , midi_middle_c
  , toMidiPitch
  
  )  where


import Payasan.Base.Pitch hiding ( middle_c )

import Data.Data
import Data.Fixed



-- | Although Midi Pitch can potentially support Num and 
-- integral classes, it shouldn't as pitch operations are not
-- really numeric.
--
-- [But a Num instance would allow us to use numeric literals]
--
newtype MidiPitch = MidiPitch { getMidiPitch :: Int }
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


newtype CpsPitch = CpsPitch { getCpsPitch :: Milli }
  deriving (Data,Eq,Ord,Show,Typeable)
  

newtype HzPitch = HzPitch { getHzPitch :: Milli }
  deriving (Data,Eq,Ord,Show,Typeable)  

cps_middle_c :: CpsPitch
cps_middle_c = CpsPitch 8.000


toCpsPitch :: Pitch -> CpsPitch
toCpsPitch (Pitch (PitchName l a) ove) = CpsPitch $ o + frac
  where
    o     = fromIntegral $ 4 + ove
    semis = fromPitchLetter l + fromAlteration a
    frac  = (realToFrac semis) / 100

toMidiPitch :: Pitch -> MidiPitch
toMidiPitch = MidiPitch . midiSemitoneCount

midi_middle_c :: MidiPitch
midi_middle_c = MidiPitch 60

-- TODO add conversions to HzPitch etc.



    