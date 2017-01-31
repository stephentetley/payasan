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
  , CpsPitch(..)
  , HzPitch(..)
  
  , midi_middle_c
  , pitchToMidiPitch
  
  , cps_middle_c
  , pitchToCpsPitch

  
  )  where


import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data
import qualified Data.Fixed as FIXED
import Data.Word



-- NOTE - should MIDI be capitalized? 
-- (If so then should Cps?)

-- | Although Midi Pitch can potentially support Num and 
-- integral classes, it shouldn't as pitch operations are not
-- really numeric.
--
-- [But a Num instance would allow us to use numeric literals]
--
newtype MidiPitch = MidiPitch { getMidiPitch :: Word8 }
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


-- | Middle C is octave 8
newtype CpsPitch = CpsPitch { getCpsPitch :: FIXED.Milli }
  deriving (Data,Eq,Ord,Show,Typeable)
  


newtype HzPitch = HzPitch { getHzPitch :: FIXED.Milli }
  deriving (Data,Eq,Ord,Show,Typeable)  



--------------------------------------------------------------------------------
-- Pretty instances are for debugging.


instance Pretty MidiPitch where 
  pPrint (MidiPitch i)   = text $ show i

instance Pretty CpsPitch where 
  pPrint (CpsPitch d)   = text $ show d
  
instance Pretty HzPitch where 
  pPrint (HzPitch d)   = text $ show d
  
  
--------------------------------------------------------------------------------
-- operations and constants  
  
cps_middle_c :: CpsPitch
cps_middle_c = CpsPitch 8.000


pitchToCpsPitch :: Pitch -> CpsPitch
pitchToCpsPitch (Pitch (PitchName l a) ove) = CpsPitch $ o + frac
  where
    o     = fromIntegral $ 4 + ove
    semis = fromPitchLetter l + fromAlteration a
    frac  = (realToFrac semis) / 100



midi_middle_c :: MidiPitch
midi_middle_c = MidiPitch 60


pitchToMidiPitch :: Pitch -> MidiPitch
pitchToMidiPitch = MidiPitch . fromIntegral . midiSemitoneCount


-- TODO - add conversions to HzPitch etc. 
-- Haskell code that I've already written exists somewhere in
-- Copperbox (Googlecode)...



    