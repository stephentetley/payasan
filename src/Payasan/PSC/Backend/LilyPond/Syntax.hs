{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Backend.LilyPond.Syntax
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist segmented into bars, with notes, rests, 
-- chords, grace notes and triplets.
--
-- Adapted syntax following Lilypond.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Backend.LilyPond.Syntax
  ( 

    LyPart1
  , LyBar1
  , LyNoteGroup1
  , LyElement1
  , LyNote1

  , LyPart2
  , LyBar2
  , LyNoteGroup2
  , LyElement2
  , LyNote2


  ) where

import Payasan.PSC.Base.LilyPondCommon
import Payasan.PSC.Repr.IRBeam.Syntax







--------------------------------------------------------------------------------
-- Syntax


type LyPart1 anno               = LyPart2        LyPitch anno
type LyBar1 anno                = LyBar2         LyPitch anno
type LyNoteGroup1 anno          = LyNoteGroup2   LyPitch anno
type LyElement1 anno            = LyElement2     LyPitch anno
type LyNote1 anno               = LyNote2        LyPitch anno


type LyPart2        pch anno    = Part        pch LyNoteLength anno
type LyBar2         pch anno    = Bar         pch LyNoteLength anno
type LyNoteGroup2   pch anno    = NoteGroup   pch LyNoteLength anno
type LyElement2     pch anno    = Element     pch LyNoteLength anno
type LyNote2        pch anno    = Note        pch LyNoteLength


