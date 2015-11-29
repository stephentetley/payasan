{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Internal.Plain
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- /Plain/ constructor syntax for fret diagrams and chords.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.FretDiagram.Internal.Plain
  ( 

    FretDiagramPhrase
  , NoteListAtom
  , fromNoteList
  , chord
  , rest
  ) where

import Payasan.LilyPond.FretDiagram.Internal.Base
import Payasan.LilyPond.FretDiagram.Internal.Interpret

import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Monophonic.Internal.Traversals
import Payasan.Base.Internal.CommonSyntax

import Payasan.Base.Duration
import Payasan.Base.Pitch

-- Use Monophonic syntax or not? - Initially do, see what happens...



type NoteListAtom = NoteGroup FretDiagram Duration ()

fromNoteList :: SectionInfo -> GuitarTuning -> [[NoteListAtom]] 
             -> FretDiagramPhrase
fromNoteList locals tuning xss = 
    mapPitchAnno (changeNote (section_key locals) tuning) $ 
        Phrase locals $ map Bar xss



chord :: FretDiagram -> Duration -> NoteListAtom
chord diag d = Atom $ Note diag d () NO_TIE

rest :: Duration -> NoteListAtom
rest d = Atom $ Rest d 

-- Use @chordTranslateToMain@ with this representation.
--
changeNote :: Key -> GuitarTuning 
              -> FretDiagram -> a0 -> ([Pitch], FretDiagram)
changeNote key tuning fd  _     = 
    let ps = interpretFretDiagram key tuning fd in (ps,fd)
