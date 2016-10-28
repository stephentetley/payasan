{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Repr.External.Syntax
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

module Payasan.Repr.External.Syntax
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

  ) where


import Payasan.Base.Internal.SyntaxCommon

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

