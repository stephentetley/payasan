{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.MainSyntax
-- Copyright   :  (c) Stephen Tetley 2015
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

module Payasan.Base.Internal.MainSyntax
  ( 
   
    Phrase(..)
  , Bar(..)
  , NoteGroup(..)
  , Element(..)
  , Note(..)

  ) where


import Payasan.Base.Internal.CommonSyntax

import Data.Data



--------------------------------------------------------------------------------
-- Syntax


data Phrase pch drn anno = Phrase { phrase_bars :: [Bar pch drn anno] }
  deriving (Data,Eq,Show,Typeable)

instance Monoid (Phrase pch drn anno) where
  mempty = Phrase []
  Phrase xs `mappend` Phrase ys = Phrase $ xs ++ ys


-- | Note Beaming is not captured in parsing.
--
data Bar pch drn anno = Bar 
    { bar_info          :: !LocalContextInfo
    , bar_elements      :: [NoteGroup pch drn anno]
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
      NoteElem      (Note pch drn) anno
    | Rest          drn
    | Skip          drn
    | Chord         [pch]          drn     anno
    | Graces        [Note pch drn]
    | Punctuation   String
  deriving (Data,Eq,Show,Typeable)


data Note pch drn = Note pch drn
  deriving (Data,Eq,Show,Typeable)

