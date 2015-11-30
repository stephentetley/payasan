{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monomorphic.Internal.Syntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Very simple symbolic notelist with notes, rests, 
-- and triplets (no chords or graces).
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.Syntax
  ( 

    StdMonoPhrase
  , StdMonoBar
  , StdMonoNoteGroup
  , StdMonoElement

  , StdMonoPhrase1
  , StdMonoBar1
  , StdMonoNoteGroup1
  , StdMonoElement1

  , StdMonoPhrase2
  , StdMonoBar2
  , StdMonoNoteGroup2
  , StdMonoElement2

  , LyMonoPhrase1
  , LyMonoBar1
  , LyMonoNoteGroup1
  , LyMonoElement1

  , LyMonoPhrase2
  , LyMonoBar2
  , LyMonoNoteGroup2
  , LyMonoElement2

  , ABCMonoPhrase
  , ABCMonoBar
  , ABCMonoNoteGroup
  , ABCMonoElement
  

  , Phrase(..)
  , Bar(..)
  , NoteGroup(..)
  , Element(..)

  , pushSectionInfo
  , sectionInfo
  , sizeNoteGroup

  ) where

import Payasan.Base.Internal.ABC.Syntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Data.Data

--------------------------------------------------------------------------------
-- Syntax


type StdMonoPhrase                  = StdMonoPhrase1    ()
type StdMonoBar                     = StdMonoBar1       ()
type StdMonoNoteGroup               = StdMonoNoteGroup1 ()
type StdMonoElement                 = StdMonoElement1   ()


type StdMonoPhrase1     anno        = Phrase    Pitch Duration anno
type StdMonoBar1        anno        = Bar       Pitch Duration anno
type StdMonoNoteGroup1  anno        = NoteGroup Pitch Duration anno
type StdMonoElement1    anno        = Element   Pitch Duration anno

type StdMonoPhrase2     pch anno    = Phrase    pch Duration anno
type StdMonoBar2        pch anno    = Bar       pch Duration anno
type StdMonoNoteGroup2  pch anno    = NoteGroup pch Duration anno
type StdMonoElement2    pch anno    = Element   pch Duration anno

type LyMonoPhrase1      anno        = LyMonoPhrase2     LyPitch anno
type LyMonoBar1         anno        = LyMonoBar2        LyPitch anno
type LyMonoNoteGroup1   anno        = LyMonoNoteGroup2  LyPitch anno
type LyMonoElement1     anno        = LyMonoElement2    LyPitch anno

type LyMonoPhrase2      pch anno    = Phrase    pch LyNoteLength anno
type LyMonoBar2         pch anno    = Bar       pch LyNoteLength anno
type LyMonoNoteGroup2   pch anno    = NoteGroup pch LyNoteLength anno
type LyMonoElement2     pch anno    = Element   pch LyNoteLength anno


type ABCMonoPhrase                  = Phrase    ABCPitch ABCNoteLength ()
type ABCMonoBar                     = Bar       ABCPitch ABCNoteLength ()
type ABCMonoNoteGroup               = NoteGroup ABCPitch ABCNoteLength ()
type ABCMonoElement                 = Element   ABCPitch ABCNoteLength ()




-- | Parametric on pitch so we can have the same syntax to 
-- represent scale degrees, drum notes, etc.
--
-- Parametric on duration so we can read ABC and decode duration
-- multipliers in a post-parsing phase.
--
-- LocalRenderInfo is annotated at the Phrase level - while this
-- prevents concatenation it simplifies transformation.
-- 
data Phrase pch drn anno = Phrase 
    { phrase_header     :: !SectionInfo
    , phrase_bars       :: [Bar pch drn anno] 
    }
  deriving (Data,Eq,Show,Typeable)



-- | Note Beaming is not captured in parsing.
--
data Bar pch drn anno = Bar 
    { bar_groups        :: [NoteGroup pch drn anno]
    }
  deriving (Data,Eq,Show,Typeable)




-- | Note Beaming is added in rendering.
--
-- Tuplets seem essential 
--
data NoteGroup pch drn anno = 
      Atom    (Element pch drn anno)
    | Tuplet  TupletSpec         [NoteGroup pch drn anno]
  deriving (Data,Eq,Show,Typeable)


-- | TODO - should we implement ties?
--
-- Note - if we ignore anno Element can also represent 
-- Maybe+duration.
--
data Element pch drn anno = 
      Note          pch   drn   anno  Tie
    | Rest          drn
    | Spacer        drn
    | Skip          drn
    | Punctuation   String
  deriving (Data,Eq,Show,Typeable)



--------------------------------------------------------------------------------
-- Push RenderInfo into bars.


pushSectionInfo :: SectionInfo 
                -> Phrase pch drn anno 
                -> Phrase pch drn anno
pushSectionInfo ri (Phrase { phrase_bars = bs }) = 
    Phrase { phrase_header = ri
           , phrase_bars   = bs }


sectionInfo :: Phrase pch drn anno -> SectionInfo
sectionInfo = phrase_header


sizeNoteGroup :: NoteGroup pch Duration anno -> RDuration
sizeNoteGroup (Atom e)          = sizeElement e
sizeNoteGroup (Tuplet spec es)  = tupletUnitRDuration spec (firstOf es)
  where
    firstOf (x:_)   = sizeNoteGroup x
    firstOf []      = toRDuration d_eighth

sizeElement :: Element pch Duration anno -> RDuration
sizeElement (Note _ d _ _)      = toRDuration d
sizeElement (Rest d)            = toRDuration d
sizeElement (Spacer d)          = toRDuration d
sizeElement (Skip d)            = toRDuration d
sizeElement (Punctuation {})    = 0


