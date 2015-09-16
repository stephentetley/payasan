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
import Payasan.Base.Internal.BeamPitchTrafo as P

import Payasan.Base.Duration
import Payasan.Base.Pitch


translate :: Phrase Pitch Duration anno -> Phrase MidiPitch Duration anno
translate = P.transform pch_algo


type PTMon   a      = P.Mon () a


--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: P.BeamPitchAlgo () Pitch MidiPitch
pch_algo = P.BeamPitchAlgo
    { P.initial_state           = ()
    , P.element_trafo           = elementP
    }


elementP :: Element Pitch drn anno -> PTMon (Element MidiPitch drn anno)
elementP (NoteElem e a)         = (\n -> NoteElem n a) <$> noteP e
elementP (Rest d)               = pure $ Rest d
elementP (Chord ps d a)         = pure $ Chord (map pitchToMidi ps) d a
elementP (Graces ns)            = Graces <$> mapM noteP ns


noteP :: Note Pitch drn -> PTMon (Note MidiPitch drn)
noteP (Note pch drn)            = pure $ Note (pitchToMidi pch) drn
