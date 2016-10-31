{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.Syntax
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist without bars.
--
--------------------------------------------------------------------------------

module Payasan.Score.Cadenza.Internal.Syntax
  ( 

    StdCadenzaPart
  , StdCadenzaNoteGroup
  , StdCadenzaElement

  , StdCadenzaPart1
  , StdCadenzaNoteGroup1
  , StdCadenzaElement1

  , StdCadenzaPart2
  , StdCadenzaNoteGroup2
  , StdCadenzaElement2

  , LyCadenzaPart1
  , LyCadenzaNoteGroup1
  , LyCadenzaElement1

  , LyCadenzaPart2
  , LyCadenzaNoteGroup2
  , LyCadenzaElement2

  , Part(..)
  , NoteGroup(..)
  , Element(..)


  , pushSectionInfo
  , sectionInfo
  , sizeNoteGroup

  ) where

import Payasan.PSC.Base.LilyPondCommon
import Payasan.PSC.Base.SyntaxCommon
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Data.Data

--------------------------------------------------------------------------------
-- Syntax



type StdCadenzaPart                     = StdCadenzaPart1      ()
type StdCadenzaNoteGroup                = StdCadenzaNoteGroup1 ()
type StdCadenzaElement                  = StdCadenzaElement1   ()


type StdCadenzaPart1       anno         = Part      Pitch Duration anno
type StdCadenzaNoteGroup1  anno         = NoteGroup Pitch Duration anno
type StdCadenzaElement1    anno         = Element   Pitch Duration anno

type StdCadenzaPart2       pch anno     = Part      pch Duration anno
type StdCadenzaNoteGroup2  pch anno     = NoteGroup pch Duration anno
type StdCadenzaElement2    pch anno     = Element   pch Duration anno

type LyCadenzaPart1        anno         = LyCadenzaPart2       LyPitch anno
type LyCadenzaNoteGroup1   anno         = LyCadenzaNoteGroup2  LyPitch anno
type LyCadenzaElement1     anno         = LyCadenzaElement2    LyPitch anno

type LyCadenzaPart2        pch anno     = Part      pch LyNoteLength anno
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
-- LocalRenderInfo is annotated at the Part level - while this
-- prevents concatenation it simplifies transformation.
-- 
data Part pch drn anno = Part 
    { part_header       :: !SectionInfo
    , part_groups       :: [NoteGroup pch drn anno] 
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
                -> Part pch drn anno 
                -> Part pch drn anno
pushSectionInfo ri (Part { part_groups = gs }) = 
    Part { part_header = ri
         , part_groups = gs }


sectionInfo :: Part pch drn anno -> SectionInfo
sectionInfo = part_header




sizeNoteGroup :: NoteGroup pch Duration anno -> RDuration
sizeNoteGroup (Atom e)          = sizeElement e
sizeNoteGroup (Beamed gs)       = sum $ map sizeNoteGroup gs
sizeNoteGroup (Tuplet spec es)  = tupletUnitRDuration spec (firstOf es)
  where
    firstOf (x:_)   = sizeElement x
    firstOf []      = toRDuration d_eighth

sizeElement :: Element pch Duration anno -> RDuration
sizeElement (Note _ d _ _)          = toRDuration d
sizeElement (Rest d)                = toRDuration d
sizeElement (Spacer d)              = toRDuration d
sizeElement (Skip d)                = toRDuration d
sizeElement (Punctuation {})        = 0


