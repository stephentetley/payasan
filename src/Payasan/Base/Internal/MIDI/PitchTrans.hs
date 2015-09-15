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
import Payasan.Base.Internal.MainSyntax

import Payasan.Base.Duration
import Payasan.Base.Pitch


translate :: Phrase Pitch Duration -> Phrase MidiPitch Duration
translate = phraseT


phraseT :: Phrase Pitch Duration ->  Phrase MidiPitch Duration
phraseT (Phrase bs)             = Phrase $ map barT bs


barT :: Bar Pitch Duration -> Bar MidiPitch Duration
barT (Bar info cs)              = Bar info $ map ctxElementT cs
     


ctxElementT :: CtxElement Pitch Duration -> CtxElement MidiPitch Duration
ctxElementT (Atom e)            = Atom $ elementT e
ctxElementT (Tuplet {})         = error $ "MidiPitchTrans - Tuplet"

elementT :: Element Pitch Duration -> Element MidiPitch Duration
elementT (NoteElem e)           = NoteElem $ noteT e
elementT (Rest d)               = Rest d
elementT (Chord ps d)           = Chord (map pitchToMidi ps) d
elementT (Graces {})            = error $ "MidiPitchTrans - Graces"


noteT :: Note Pitch Duration -> Note MidiPitch Duration
noteT (Note pch drn)            = Note (pitchToMidi pch) drn



