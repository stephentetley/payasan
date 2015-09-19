{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Percussion.Internal.PitchTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert DrumPitch to MidiPitch.
-- 
--------------------------------------------------------------------------------

module Payasan.Percussion.Internal.PitchTrans
  ( 
    translate
  ) where

import Payasan.Percussion.Internal.Base

import Payasan.Base.Internal.MIDI.PrimitiveSyntax (MidiPitch)
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals

import Payasan.Base.Duration


translate :: Phrase DrumPitch Duration anno -> Phrase MidiPitch Duration anno
translate = transformP pch_algo


type PTMon   a      = Mon () a


--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: BeamPitchAlgo () DrumPitch MidiPitch
pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = elementP
    }


elementP :: Element DrumPitch drn anno -> PTMon (Element MidiPitch drn anno)
elementP (NoteElem e a)         = (\n -> NoteElem n a) <$> noteP e
elementP (Rest d)               = pure $ Rest d
elementP (Chord ps d a)         = pure $ Chord (map toMidiPitch ps) d a
elementP (Graces ns)            = Graces <$> mapM noteP ns


noteP :: Note DrumPitch drn -> PTMon (Note MidiPitch drn)
noteP (Note pch drn)            = pure $ Note (toMidiPitch pch) drn
