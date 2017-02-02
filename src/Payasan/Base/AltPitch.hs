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
  , OctPitch(..)
  , HzPitch(..)
  
  , midi_middle_c
  , pitchToMidiPitch
  
  , hz_middle_c
  , pitchToHzPitch

  , oct_middle_c
  , pitchToOctPitch
  
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


-- | Middle C is octave 8
-- Octave fractions - e.g. 8.0, 8.1667, 8.3334 ..
newtype OctPitch = OctPitch { getOctPitch :: FIXED.Milli }
  deriving (Data,Eq,Ord,Show,Typeable)
  


newtype HzPitch = HzPitch { getHzPitch :: FIXED.Milli }
  deriving (Data,Eq,Ord,Show,Typeable)  



--------------------------------------------------------------------------------
-- Pretty instances are for debugging.


instance Pretty MidiPitch where 
  pPrint (MidiPitch i)   = text $ show i

instance Pretty OctPitch where 
  pPrint (OctPitch d)   = text $ show d
  
instance Pretty HzPitch where 
  pPrint (HzPitch d)   = text $ show d
  
  
--------------------------------------------------------------------------------
-- operations and constants  

midiOct :: MidiPitch -> OctPitch
midiOct m = OctPitch $ 8.0 + (realToFrac $ getMidiPitch m - 60)/12.0
  

hz_middle_c :: HzPitch
hz_middle_c = HzPitch 261.625565


-- This is likely wrong - it was copied in from somewhere and is probably 
-- oct(-ave fraction)... 
pitchToHzPitch :: Pitch -> HzPitch
pitchToHzPitch (Pitch (PitchName l a) ove) = HzPitch $ o + frac
  where
    o     = fromIntegral $ 4 + ove
    semis = fromPitchLetter l + fromAlteration a
    frac  = (realToFrac semis) / 100



midi_middle_c :: MidiPitch
midi_middle_c = MidiPitch 60


pitchToMidiPitch :: Pitch -> MidiPitch
pitchToMidiPitch = MidiPitch . fromIntegral . midiSemitoneCount



oct_middle_c :: OctPitch
oct_middle_c = OctPitch 8.000


pitchToOctPitch :: Pitch -> OctPitch
pitchToOctPitch = midiOct . pitchToMidiPitch


-- TODO - add conversions to HzPitch etc. 
-- Haskell code that I've already written exists somewhere in
-- Copperbox (Googlecode)...



    