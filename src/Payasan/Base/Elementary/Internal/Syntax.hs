{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.Syntax
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

module Payasan.Base.Elementary.Internal.Syntax
  ( 

    StdElemPhrase
  , StdElemBar
  , StdElemNoteGroup
  , StdElemElement

  , StdElemPhrase1
  , StdElemBar1
  , StdElemNoteGroup1
  , StdElemElement1

  , StdElemPhrase2
  , StdElemBar2
  , StdElemNoteGroup2
  , StdElemElement2

  , LyElemPhrase1
  , LyElemBar1
  , LyElemNoteGroup1
  , LyElemElement1

  , LyElemPhrase2
  , LyElemBar2
  , LyElemNoteGroup2
  , LyElemElement2

  , ABCElemPhrase
  , ABCElemBar
  , ABCElemNoteGroup
  , ABCElemElement
  

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


type StdElemPhrase                  = StdElemPhrase1    ()
type StdElemBar                     = StdElemBar1       ()
type StdElemNoteGroup               = StdElemNoteGroup1 ()
type StdElemElement                 = StdElemElement1   ()


type StdElemPhrase1     anno        = Phrase    Pitch Duration anno
type StdElemBar1        anno        = Bar       Pitch Duration anno
type StdElemNoteGroup1  anno        = NoteGroup Pitch Duration anno
type StdElemElement1    anno        = Element   Pitch Duration anno

type StdElemPhrase2     pch anno    = Phrase    pch Duration anno
type StdElemBar2        pch anno    = Bar       pch Duration anno
type StdElemNoteGroup2  pch anno    = NoteGroup pch Duration anno
type StdElemElement2    pch anno    = Element   pch Duration anno

type LyElemPhrase1      anno        = LyElemPhrase2     LyPitch anno
type LyElemBar1         anno        = LyElemBar2        LyPitch anno
type LyElemNoteGroup1   anno        = LyElemNoteGroup2  LyPitch anno
type LyElemElement1     anno        = LyElemElement2    LyPitch anno

type LyElemPhrase2      pch anno    = Phrase    pch LyNoteLength anno
type LyElemBar2         pch anno    = Bar       pch LyNoteLength anno
type LyElemNoteGroup2   pch anno    = NoteGroup pch LyNoteLength anno
type LyElemElement2     pch anno    = Element   pch LyNoteLength anno


type ABCElemPhrase                  = Phrase    ABCPitch ABCNoteLength ()
type ABCElemBar                     = Bar       ABCPitch ABCNoteLength ()
type ABCElemNoteGroup               = NoteGroup ABCPitch ABCNoteLength ()
type ABCElemElement                 = Element   ABCPitch ABCNoteLength ()




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




-- | Note - Beaming is added in rendering.
--
-- Tuplets seem essential - but perhaps they do not need 
-- nesting...
--
data NoteGroup pch drn anno = 
      Atom    (Element pch drn anno)
    | Tuplet  TupletSpec         [Element pch drn anno]
  deriving (Data,Eq,Show,Typeable)


-- | Note - unfortunately Spacers and Skips are interpreted
-- differently by LilyPond hence we must distinguish them.
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
    firstOf (e:_)   = sizeElement e
    firstOf []      = toRDuration d_eighth

sizeElement :: Element pch Duration anno -> RDuration
sizeElement (Note _ d _ _)      = toRDuration d
sizeElement (Rest d)            = toRDuration d
sizeElement (Spacer d)          = toRDuration d
sizeElement (Skip d)            = toRDuration d
sizeElement (Punctuation {})    = 0


