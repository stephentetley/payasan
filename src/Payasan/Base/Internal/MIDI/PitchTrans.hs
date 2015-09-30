{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.MIDI.PitchTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Main syntax to MIDI syntax.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.MIDI.PitchTrans
  ( 
    translate
  ) where


import Payasan.Base.Internal.MIDI.PrimitiveSyntax (MidiPitch, pitchToMidi)
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals

import Payasan.Base.Duration
import Payasan.Base.Pitch


translate :: Phrase Pitch Duration anno -> Phrase MidiPitch Duration anno
translate = transformP pch_algo


type PTMon   a      = Mon () a


--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: BeamPitchAlgo () Pitch MidiPitch
pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = elementP
    }


elementP :: Element Pitch drn anno -> PTMon (Element MidiPitch drn anno)
elementP (NoteElem e a)         = (\n -> NoteElem n a) <$> noteP e
elementP (Rest d)               = pure $ Rest d
elementP (Chord ps d a)         = pure $ Chord (map pitchToMidi ps) d a
elementP (Graces ns)            = Graces <$> mapM noteP ns
elementP (Punctuation s)        = pure $ Punctuation s


noteP :: Note Pitch drn -> PTMon (Note MidiPitch drn)
noteP (Note pch drn)            = pure $ Note (pitchToMidi pch) drn
