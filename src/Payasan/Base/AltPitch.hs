{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.AltPitch
-- Copyright   :  (c) Stephen Tetley 2016-2017
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
  , HertzPitch(..)
  , PCPitch(..)
  
  , midi_middle_c
  , pitchToMidiPitch
  
  , hertz_middle_c
  , pitchToHertzPitch

  , octave_pitch_middle_c
  , pitchToPCPitch
  
  )  where


import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data
import qualified Data.Fixed as FIXED
import Data.Word



-- NOTE - should MIDI be capitalized? 
-- (If so then should Hz?)

-- | Although Midi Pitch can potentially support Num and 
-- integral classes, it shouldn't as pitch operations are not
-- really numeric.
--
-- [But a Num instance would allow us to use numeric literals]
--
newtype MidiPitch = MidiPitch { getMidiPitch :: Word8 }
  deriving (Data,Enum,Eq,Ord,Show,Typeable)



newtype HertzPitch = HertzPitch { getHertzPitch :: FIXED.Micro }
  deriving (Data,Eq,Ord,Show,Typeable)  


-- | Middle C is octave 8
-- Octave fractions - e.g. 8.0, 8.01, 8.02 ..
newtype PCPitch = PCPitch { getPCPitch :: FIXED.Centi }
  deriving (Data,Eq,Ord,Show,Typeable)
  



--------------------------------------------------------------------------------
-- Pretty instances are for debugging.


instance Pretty MidiPitch where 
  pPrint (MidiPitch i)   = text $ show i

instance Pretty PCPitch where 
  pPrint (PCPitch d)   = text $ show d
  
instance Pretty HertzPitch where 
  pPrint (HertzPitch d)   = text $ show d
  
  
--------------------------------------------------------------------------------
-- MidiPitch

midi_middle_c :: MidiPitch
midi_middle_c = MidiPitch 60


pitchToMidiPitch :: Pitch -> MidiPitch
pitchToMidiPitch = MidiPitch . fromIntegral . midiSemitoneCount


  

--------------------------------------------------------------------------------
-- HertzPitch

midiHz :: MidiPitch -> Double
midiHz m = 6.875 * (2.0 ** ((m' +3) / 12)) 
  where
    m' = realToFrac $ getMidiPitch m


hertz_middle_c :: HertzPitch
hertz_middle_c = HertzPitch 261.625565


-- This is likely wrong - it was copied in from somewhere and is probably 
-- oct(-ave fraction)... 
pitchToHertzPitch :: Pitch -> HertzPitch
pitchToHertzPitch = HertzPitch . realToFrac . midiHz . pitchToMidiPitch




--------------------------------------------------------------------------------
-- PCPitch


-- midiPC :: MidiPitch -> PCPitch
-- midiPC m = PCPitch $ 8.0 + (realToFrac $ getMidiPitch m - 60)/12.0


octave_pitch_middle_c :: PCPitch
octave_pitch_middle_c = PCPitch 8.000


pitchToPCPitch :: Pitch -> PCPitch
pitchToPCPitch (Pitch (PitchName l a) ove) = PCPitch $ o + frac
  where
    o     = fromIntegral $ 4 + ove
    semis = fromPitchLetter l + fromAlteration a
    frac  = (realToFrac semis) / 100




    