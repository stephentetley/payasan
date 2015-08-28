{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.MidiPitchTrans
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

module Payasan.Base.Internal.MidiPitchTrans
  ( 
    translate
  ) where


import Payasan.Base.Internal.MainSyntax
import qualified Payasan.Base.Internal.MidiSyntax as T

import Payasan.Base.Duration
import Payasan.Base.Pitch


translate :: Phrase Pitch Duration -> Phrase T.MidiPitch Duration
translate = phraseT


phraseT :: Phrase Pitch Duration ->  Phrase T.MidiPitch Duration
phraseT (Phrase bs)             = Phrase $ map barT bs


barT :: Bar Pitch Duration -> Bar T.MidiPitch Duration
barT (Bar info cs)              = Bar info $ map ctxElementT cs
     


ctxElementT :: CtxElement Pitch Duration -> CtxElement T.MidiPitch Duration
ctxElementT (Atom e)            = Atom $ elementT e
ctxElementT (Tuplet {})         = error $ "MidiPitchTrans - Tuplet"

elementT :: Element Pitch Duration -> Element T.MidiPitch Duration
elementT (NoteElem e)           = NoteElem $ noteT e
elementT (Rest d)               = Rest d
elementT (Chord ps d)           = Chord (map T.pitchToMidi ps) d
elementT (Graces {})            = error $ "MidiPitchTrans - Graces"


noteT :: Note Pitch Duration -> Note T.MidiPitch Duration
noteT (Note pch drn)            = Note (T.pitchToMidi pch) drn



