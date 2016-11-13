{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Backend.MIDI.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Pitch to MidiPitch and Duration to RDuration.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Backend.MIDI.OutTrans
  ( 
    translateToMidiP
  ) where


import Payasan.PSC.Backend.MIDI.PrimitiveSyntax (MidiPitch, pitchToMidi)
import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.Base.Pitch


translateToMidiP :: Part Pitch drn anno -> Part MidiPitch drn anno
translateToMidiP = transformP pch_algo


-- No need for a Duration translation. This is done together with
-- coalescing ties.



--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: BeamPitchAlgo () Pitch MidiPitch
pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = liftElementTrafo elementP
    }


elementP :: Element Pitch drn anno -> Element MidiPitch drn anno
elementP (NoteElem e a t)       = NoteElem (noteP e) a t
elementP (Rest d)               = Rest d
elementP (Spacer d)             = Spacer d
elementP (Skip d)               = Skip d
elementP (Chord ps d a t)       = Chord (map pitchToMidi ps) d a t
elementP (Graces ns)            = Graces $ map noteP ns
elementP (Punctuation s)        = Punctuation s


noteP :: Note Pitch drn -> Note MidiPitch drn
noteP (Note pch drn)            = Note (pitchToMidi pch) drn


