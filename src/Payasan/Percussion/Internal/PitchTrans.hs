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
import Payasan.Base.Internal.BeamPitchTrafo as P

import Payasan.Base.Duration


translate :: Phrase DrumPitch Duration -> Phrase MidiPitch Duration
translate = P.transform pch_algo


type PTMon   a      = P.Mon () a


--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: P.BeamPitchAlgo () DrumPitch MidiPitch
pch_algo = P.BeamPitchAlgo
    { P.initial_state           = ()
    , P.element_trafo           = elementP
    }


elementP :: Element DrumPitch drn -> PTMon (Element MidiPitch drn)
elementP (NoteElem a)           = NoteElem <$> noteP a
elementP (Rest d)               = pure $ Rest d
elementP (Chord ps d)           = pure $ Chord (map toMidiPitch ps) d
elementP (Graces ns)            = Graces <$> mapM noteP ns


noteP :: Note DrumPitch drn -> PTMon (Note MidiPitch drn)
noteP (Note pch drn)            = pure $ Note (toMidiPitch pch) drn
