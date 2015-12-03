{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Cadenza.Internal.Syntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist without bars.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Cadenza.Internal.Syntax
  ( 

    StdCadenzaPhrase
  , StdCadenzaNoteGroup
  , StdCadenzaElement

  , StdCadenzaPhrase1
  , StdCadenzaNoteGroup1
  , StdCadenzaElement1

  , StdCadenzaPhrase2
  , StdCadenzaNoteGroup2
  , StdCadenzaElement2

  , LyCadenzaPhrase1
  , LyCadenzaNoteGroup1
  , LyCadenzaElement1

  , LyCadenzaPhrase2
  , LyCadenzaNoteGroup2
  , LyCadenzaElement2

  , Phrase(..)
  , NoteGroup(..)
  , Element(..)


  , pushSectionInfo
  , sectionInfo
  , sizeNoteGroup

  ) where

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Data.Data

--------------------------------------------------------------------------------
-- Syntax



type StdCadenzaPhrase                   = StdCadenzaPhrase1    ()
type StdCadenzaNoteGroup                = StdCadenzaNoteGroup1 ()
type StdCadenzaElement                  = StdCadenzaElement1   ()


type StdCadenzaPhrase1     anno         = Phrase    Pitch Duration anno
type StdCadenzaNoteGroup1  anno         = NoteGroup Pitch Duration anno
type StdCadenzaElement1    anno         = Element   Pitch Duration anno

type StdCadenzaPhrase2     pch anno     = Phrase    pch Duration anno
type StdCadenzaNoteGroup2  pch anno     = NoteGroup pch Duration anno
type StdCadenzaElement2    pch anno     = Element   pch Duration anno

type LyCadenzaPhrase1      anno         = LyCadenzaPhrase2     LyPitch anno
type LyCadenzaNoteGroup1   anno         = LyCadenzaNoteGroup2  LyPitch anno
type LyCadenzaElement1     anno         = LyCadenzaElement2    LyPitch anno

type LyCadenzaPhrase2      pch anno     = Phrase    pch LyNoteLength anno
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
-- LocalRenderInfo is annotated at the Phrase level - while this
-- prevents concatenation it simplifies transformation.
-- 
data Phrase pch drn anno = Phrase 
    { phrase_header     :: !SectionInfo
    , phrase_groups     :: [NoteGroup pch drn anno] 
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
                -> Phrase pch drn anno 
                -> Phrase pch drn anno
pushSectionInfo ri (Phrase { phrase_groups = gs }) = 
    Phrase { phrase_header = ri
           , phrase_groups = gs }


sectionInfo :: Phrase pch drn anno -> SectionInfo
sectionInfo = phrase_header




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


