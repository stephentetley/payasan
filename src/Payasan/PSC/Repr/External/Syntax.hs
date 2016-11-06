{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.External.Syntax
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
-- Parametric on Duration and Pitch.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.External.Syntax
  ( 
   
    StdPart
  , StdBar
  , StdNoteGroup
  , StdElement
  , StdNote

  , StdPart1
  , StdBar1
  , StdNoteGroup1
  , StdElement1

  , Part(..)
  , Bar(..)
  , NoteGroup(..)
  , Element(..)
  , Note(..)

  , LyPart1
  , LyBar1
  , LyNoteGroup1
  , LyElement1
  , LyNote1

  , LyPart2
  , LyBar2
  , LyNoteGroup2
  , LyElement2
  , LyNote2

  , ABCPart
  , ABCBar
  , ABCNoteGroup
  , ABCElement
  , ABCNote

  , ABCPart1
  , ABCBar1
  , ABCNoteGroup1
  , ABCElement1

  -- * Operations
  , pushSectionInfo


  ) where


import Payasan.PSC.Base.ABCCommon
import Payasan.PSC.Base.LilyPondCommon
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch

import Data.Data



--------------------------------------------------------------------------------
-- Syntax


type StdPart            = Part      Pitch Duration () 
type StdBar             = Bar       Pitch Duration () 
type StdNoteGroup       = NoteGroup Pitch Duration () 
type StdElement         = Element   Pitch Duration ()
type StdNote            = Note      Pitch Duration

type StdPart1 anno      = Part      Pitch Duration anno
type StdBar1 anno       = Bar       Pitch Duration anno
type StdNoteGroup1 anno = NoteGroup Pitch Duration anno
type StdElement1 anno   = Element   Pitch Duration anno




data Part pch drn anno = Part { part_bars :: [Bar pch drn anno] }
  deriving (Data,Eq,Show,Typeable)

instance Monoid (Part pch drn anno) where
  mempty = Part []
  Part xs `mappend` Part ys = Part $ xs ++ ys


-- | Note Beaming is not captured in parsing.
--
data Bar pch drn anno = Bar 
    { bar_info          :: !SectionInfo
    , bar_groups        :: [NoteGroup pch drn anno]
    }
  deriving (Data,Eq,Show,Typeable)

-- | Note Beaming is not captured in parsing.
--
data NoteGroup pch drn anno = 
      Atom     (Element pch drn anno)
    | Tuplet   TupletSpec        [NoteGroup pch drn anno]
  deriving (Data,Eq,Show,Typeable)


-- | Note is should be quite easy to add ties (as write-only)
-- to get long notes after beaming.
--
-- See old Neume code. 
--
-- Punctuation is for LilyPond only (may change).
-- 
-- Skip is essentially a rest but they have different 
-- interpretations in LilyPond and need to be 
-- differentiated.
--
data Element pch drn anno = 
      NoteElem      (Note pch drn) anno   Tie
    | Rest          drn
    | Spacer        drn
    | Skip          drn
    | Chord         [pch]          drn    anno   Tie
    | Graces        [Note pch drn]
    | Punctuation   String
  deriving (Data,Eq,Show,Typeable)


data Note pch drn = Note pch drn
  deriving (Data,Eq,Show,Typeable)

--------------------------------------------------------------------------------
-- LilyPond Aliases


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


--------------------------------------------------------------------------------
-- Syntax

-- | ABC is not annotated, though polymorphic anno is used
-- so that /more/ syntax can be printed.

type ABCPart                    = ABCPart1      ()
type ABCBar                     = ABCBar1       ()      
type ABCNoteGroup               = ABCNoteGroup1 ()
type ABCElement                 = ABCElement1   ()
type ABCNote                    = Note     ABCPitch ABCNoteLength


-- Gen- prefix indicates the must general syntax allowed.

type ABCPart1 anno              = Part        ABCPitch ABCNoteLength anno
type ABCBar1 anno               = Bar         ABCPitch ABCNoteLength anno
type ABCNoteGroup1 anno         = NoteGroup   ABCPitch ABCNoteLength anno
type ABCElement1 anno           = Element     ABCPitch ABCNoteLength anno


--------------------------------------------------------------------------------
-- Operations

pushSectionInfo :: SectionInfo 
                -> Part pch drn anno 
                -> Part pch drn anno
pushSectionInfo si (Part bs) = Part $ map upd bs
  where
    upd bar = bar { bar_info = si }
