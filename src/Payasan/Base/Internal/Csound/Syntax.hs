{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Csound.Syntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist segmented into bars, with notes, rests, 
-- chords, grace notes and triplets.
--
-- Concrete syntax following Csound notelists.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Csound.Syntax
  ( 

    CsPhrase
  , CsBar
  , CsNoteGroup
  , CsElement
  , CsNote

  , CsPhrase1
  , CsBar1
  , CsNoteGroup1
  , CsElement1


  , CpsPitch(..)

  , middle_c

  , toCpsPitch
  , toSeconds

  ) where

import Payasan.Base.Internal.Base
import qualified Payasan.Base.Internal.BeamSyntax as BEAM

import Payasan.Base.Duration
import Payasan.Base.Pitch hiding ( middle_c )

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data
import Data.Fixed

--------------------------------------------------------------------------------
-- Syntax

-- | TODO - annos potentially allow expressive variation in 
-- notelist output.

type CsPhrase                   = CsPhrase1     ()
type CsBar                      = CsBar1        ()      
type CsNoteGroup                = CsNoteGroup1  ()
type CsElement                  = CsElement1    ()
type CsNote                     = BEAM.Note     CpsPitch Seconds


type CsPhrase1 anno             = BEAM.Phrase     CpsPitch Seconds anno
type CsBar1 anno                = BEAM.Bar        CpsPitch Seconds anno
type CsNoteGroup1 anno          = BEAM.NoteGroup  CpsPitch Seconds anno
type CsElement1 anno            = BEAM.Element    CpsPitch Seconds anno



-- Use CpsPch - we can always print it as Hz if prefered.
-- 
data CpsPitch = CpsPitch Milli
  deriving (Data,Eq,Ord,Show,Typeable)





middle_c :: CpsPitch
middle_c = CpsPitch 8.000



toCpsPitch :: Pitch -> CpsPitch
toCpsPitch (Pitch (PitchName l a) ove) = CpsPitch $ o + frac
  where
    o    = fromIntegral $ 4 + ove
    semis = fromPitchLetter l + fromAlteration a
    frac = (realToFrac semis) / 100



toSeconds :: BPM -> Duration -> Seconds
toSeconds bpm a = qnToSeconds bpm $ 4.0 * toRDuration a


-- 1.0 represents a quarter note
qnToSeconds :: BPM -> Rational -> Seconds
qnToSeconds bpm a = let dwn_secs = (realToFrac $ 60 / bpm)
                    in realToFrac a * dwn_secs


--------------------------------------------------------------------------------
-- Pretty instances are for debugging.

instance Pretty CpsPitch where 
  pPrint (CpsPitch a)   = text $ show a



