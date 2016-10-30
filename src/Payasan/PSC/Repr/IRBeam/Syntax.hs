{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IRBeam.Syntax
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
-- Intermediate syntax for beam grouping.
--
-- Parametric on pitch for LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IRBeam.Syntax
  ( 

  -- * Common Aliases
    StdBeamPart
  , StdBeamBar
  , StdBeamNoteGroup
  , StdBeamElement
  , StdNote

  , StdBeamPart1
  , StdBeamBar1
  , StdBeamNoteGroup1
  , StdBeamElement1
  
  -- * Syntax
  , Part(..)
  , Bar(..)
  , NoteGroup(..)
  , Element(..)
  , Note(..)

  -- * Operations
  , pushSectionInfo
  , sizeNoteGroup
  , firstSectionInfo 


  ) where


import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch


import Data.Data

type StdBeamPart                = StdBeamPart1    ()
type StdBeamBar                 = StdBeamBar1       ()
type StdBeamNoteGroup           = StdBeamNoteGroup1 ()
type StdBeamElement             = StdBeamElement1   ()
type StdNote                    = Note Pitch Duration

type StdBeamPart1 anno          = Part      Pitch Duration anno
type StdBeamBar1 anno           = Bar       Pitch Duration anno
type StdBeamNoteGroup1 anno     = NoteGroup Pitch Duration anno
type StdBeamElement1 anno       = Element   Pitch Duration anno

--------------------------------------------------------------------------------
-- Syntax

-- | Beam syntax must be parametric on pitch so it can
-- handle nice LilyPond things like drums.
--
-- Also needs to be parametric on duration to handle 
-- Lilypond and ABC duration representations. 
--
data Part pch drn anno = Part { part_bars :: [Bar pch drn anno] }
  deriving (Data,Eq,Show,Typeable)


instance Monoid (Part pch drn anno) where
  mempty = Part []
  Part xs `mappend` Part ys = Part $ xs ++ ys



-- | Note Beaming is not captured in parsing.
--
data Bar pch drn anno = Bar 
    { bar_header        :: SectionInfo
    , bar_groups        :: [NoteGroup pch drn anno]
    }
  deriving (Data,Eq,Show,Typeable)

-- | Note - Beaming is not captured in parsing, but it is 
-- synthesized again for output.
--
-- Beams must allow nesting 
--
data NoteGroup pch drn anno = 
      Atom     (Element pch drn anno)
    | Beamed   [NoteGroup pch drn anno]
    | Tuplet   TupletSpec            [NoteGroup pch drn anno]
  deriving (Data,Eq,Show,Typeable)


-- | Note is should be quite easy to add ties (as write-only)
-- to get long notes after beaming.
--
-- See old Neume code. 
--
-- Punctuation is for LilyPond only (may change).
--
data Element pch drn anno = 
      NoteElem      (Note pch drn)  anno  Tie
    | Rest          drn
    | Spacer        drn
    | Skip          drn
    | Chord         [pch]           drn   anno  Tie
    | Graces        [Note pch drn]
    | Punctuation   String
  deriving (Data,Eq,Show,Typeable)


data Note pch drn = Note pch drn
  deriving (Data,Eq,Show,Typeable)

--------------------------------------------------------------------------------
-- Operations (maybe should be in another module)

pushSectionInfo :: SectionInfo 
                -> Part pch drn anno 
                -> Part pch drn anno
pushSectionInfo ri (Part bs) = Part $ map upd bs
  where
    upd bar = bar { bar_header = ri }


sizeNoteGroup :: NoteGroup pch Duration anno -> RDuration
sizeNoteGroup (Atom e)              = sizeElement e
sizeNoteGroup (Beamed es)           = sum $ map sizeNoteGroup es
sizeNoteGroup (Tuplet spec es)      = tupletUnitRDuration spec (firstOf es)
  where
    firstOf (x:_)   = sizeNoteGroup x
    firstOf []      = toRDuration d_eighth

sizeElement :: Element pch Duration anno -> RDuration
sizeElement (NoteElem (Note _ d) _ _)   = toRDuration d
sizeElement (Rest d)                    = toRDuration d
sizeElement (Spacer d)                  = toRDuration d
sizeElement (Skip d)                    = toRDuration d
sizeElement (Chord _ d _ _)             = toRDuration d
sizeElement (Graces {})                 = 0
sizeElement (Punctuation {})            = 0

firstSectionInfo :: Part pch drn anno -> Maybe SectionInfo
firstSectionInfo (Part [])    = Nothing
firstSectionInfo (Part (b:_)) = Just $ bar_header b
