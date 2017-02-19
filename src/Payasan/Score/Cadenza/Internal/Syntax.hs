{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.Syntax
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist without bars, but with beam groups.
--
--------------------------------------------------------------------------------

module Payasan.Score.Cadenza.Internal.Syntax
  ( 

    StdCadenzaSection
  , StdCadenzaNoteGroup
  , StdCadenzaElement

  , StdCadenzaSection1
  , StdCadenzaNoteGroup1
  , StdCadenzaElement1

  , StdCadenzaSection2
  , StdCadenzaNoteGroup2
  , StdCadenzaElement2

  , LyCadenzaSection1
  , LyCadenzaNoteGroup1
  , LyCadenzaElement1

  , LyCadenzaSection2
  , LyCadenzaNoteGroup2
  , LyCadenzaElement2

  , Section(..)
  , NoteGroup(..)
  , Element(..)


  , pushSectionInfo
  , sectionInfo
  , sizeNoteGroup

  ) where

import Payasan.PSC.LilyPond.Common

import Payasan.PSC.Base.SyntaxCommon
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Data.Data

--------------------------------------------------------------------------------
-- Syntax



type StdCadenzaSection                  = StdCadenzaSection1      ()
type StdCadenzaNoteGroup                = StdCadenzaNoteGroup1 ()
type StdCadenzaElement                  = StdCadenzaElement1   ()


type StdCadenzaSection1    anno         = Section   Pitch Duration anno
type StdCadenzaNoteGroup1  anno         = NoteGroup Pitch Duration anno
type StdCadenzaElement1    anno         = Element   Pitch Duration anno

type StdCadenzaSection2    pch anno     = Section   pch Duration anno
type StdCadenzaNoteGroup2  pch anno     = NoteGroup pch Duration anno
type StdCadenzaElement2    pch anno     = Element   pch Duration anno

type LyCadenzaSection1     anno         = LyCadenzaSection2    LyPitch anno
type LyCadenzaNoteGroup1   anno         = LyCadenzaNoteGroup2  LyPitch anno
type LyCadenzaElement1     anno         = LyCadenzaElement2    LyPitch anno

type LyCadenzaSection2     pch anno     = Section   pch LyNoteLength anno
type LyCadenzaNoteGroup2   pch anno     = NoteGroup pch LyNoteLength anno
type LyCadenzaElement2     pch anno     = Element   pch LyNoteLength anno


-- NOTE - we could accommodate ABC but parsing the input syntax
-- is thorny (whitespace sensitive) and ABC is already a second 
-- class citizen in Payasan because it has no rival to all the
-- nice modes in LilyPond (percussion etc.).



-- | Parametric on pitch so we can have the same syntax to 
-- represent scale degrees, drum notes, etc.
--
-- Parametric on duration so we can read LilyPond and decode
-- omitted durations in a post-parsing phase.
--
-- LocalRenderInfo is annotated at the Section level - while this
-- prevents concatenation it simplifies transformation.
-- 
data Section pch drn anno = Section
    { section_info      :: !SectionInfo
    , section_groups    :: [NoteGroup pch drn anno] 
    }
  deriving (Data,Eq,Show,Typeable)





-- | Beaming should be hand coded.
--
-- Tuplets are simplified - no nesting.
--
data NoteGroup pch drn anno = 
      Atom    (Element pch drn anno)
    | Beamed  [NoteGroup pch drn anno]
    | Tuplet  TupletSpec         [Element pch drn anno]
  deriving (Data,Eq,Show,Typeable)


-- Element currently follows Elementary syntax (omitting chords 
-- and graces).
--
data Element pch drn anno = 
      Note          pch drn   anno  Tie
    | Rest          drn
    | Spacer        drn
    | Skip          drn
    | Punctuation   String
  deriving (Data,Eq,Show,Typeable)



--------------------------------------------------------------------------------
-- Push SectionInfo into a phrase.


pushSectionInfo :: SectionInfo 
                -> Section pch drn anno 
                -> Section pch drn anno
pushSectionInfo info s = s { section_info = info }


sectionInfo :: Section pch drn anno -> SectionInfo
sectionInfo = section_info




sizeNoteGroup :: NoteGroup pch Duration anno -> RatDuration
sizeNoteGroup (Atom e)          = sizeElement e
sizeNoteGroup (Beamed gs)       = sum $ map sizeNoteGroup gs
sizeNoteGroup (Tuplet spec es)  = tupletUnitRatDuration spec (firstOf es)
  where
    firstOf (x:_)   = sizeElement x
    firstOf []      = durationToRatDuration d_eighth

sizeElement :: Element pch Duration anno -> RatDuration
sizeElement (Note _ d _ _)          = durationToRatDuration d
sizeElement (Rest d)                = durationToRatDuration d
sizeElement (Spacer d)              = durationToRatDuration d
sizeElement (Skip d)                = durationToRatDuration d
sizeElement (Punctuation {})        = 0


