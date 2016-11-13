{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Percussion.Internal.PitchTrans
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert DrumPitch to MidiPitch.
-- 
--------------------------------------------------------------------------------

module Payasan.LilyPond.Percussion.Internal.PitchTrans
  ( 
    translate
  ) where

import Payasan.LilyPond.Percussion.Internal.Base

import Payasan.PSC.Backend.MIDI.PrimitiveSyntax (MidiPitch)
import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.Base.Duration


translate :: Part DrumPitch Duration anno -> Part MidiPitch Duration anno
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
elementP (NoteElem e a t)       = (\n -> NoteElem n a t) <$> noteP e
elementP (Rest d)               = pure $ Rest d
elementP (Spacer d)             = pure $ Spacer d
elementP (Skip d)               = pure $ Skip d
elementP (Chord ps d a t)       = pure $ Chord (map toMidiPitch ps) d a t
elementP (Graces ns)            = Graces <$> mapM noteP ns
elementP (Punctuation s)        = pure $ Punctuation s


noteP :: Note DrumPitch drn -> PTMon (Note MidiPitch drn)
noteP (Note pch drn)            = pure $ Note (toMidiPitch pch) drn
