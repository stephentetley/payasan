{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.MIDI.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Pitch to MidiPitch and Duration to RDuration.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.MIDI.OutTrans
  ( 
    translateToMidiPD
  , translateToMidiD
  ) where


import Payasan.Base.Internal.MIDI.PrimitiveSyntax (MidiPitch, pitchToMidi)
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals

import Payasan.Base.Duration
import Payasan.Base.Pitch


translateToMidiPD :: Phrase Pitch Duration anno -> Phrase MidiPitch RDuration anno
translateToMidiPD = transformD drn_algo . transformP pch_algo

translateToMidiD :: Phrase pch Duration anno -> Phrase pch RDuration anno
translateToMidiD = transformD drn_algo




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


--------------------------------------------------------------------------------
-- Duration translation


drn_algo :: BeamDurationAlgo () Duration RDuration
drn_algo = BeamDurationAlgo
    { initial_stateD    = ()
    , element_trafoD    = liftElementTrafo elementD
    }


elementD :: Element pch Duration anno -> Element pch RDuration anno
elementD (NoteElem e a t)       = NoteElem (noteD e) a t
elementD (Rest d)               = Rest      $ toRDuration d
elementD (Spacer d)             = Spacer    $ toRDuration d
elementD (Skip d)               = Skip      $ toRDuration d
elementD (Chord ps d a t)       = Chord ps (toRDuration d) a t
elementD (Graces ns)            = Graces    $ map noteD ns
elementD (Punctuation s)        = Punctuation s


noteD :: Note pch Duration -> Note pch RDuration
noteD (Note pch drn)            = Note pch $ toRDuration drn
