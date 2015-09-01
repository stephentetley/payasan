{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.Syntax
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
-- Concrete syntax following ABC.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.Syntax
  ( 
    module Payasan.Base.Internal.CommonSyntax

  , LyPhrase(..)
  , Bar(..)
  , CtxElement(..)
  , LyTupletSpec(..)
  , Element(..)
  , Note(..)
  , Accidental(..)
  , PitchLetter(..)
  , Octave(..)
  , Pitch(..)
  , NoteLength(..)

  ) where

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Duration

import Data.Data

--------------------------------------------------------------------------------
-- Syntax



-- | We use the Ly- prefix here because the type may be exposed 
-- in user code and we don\'t want a name clash.
--
data LyPhrase = LyPhrase { phrase_bars :: [Bar] }
  deriving (Data,Eq,Show,Typeable)




data Bar = Bar 
    { render_info       :: LocalRenderInfo
    , bar_elements      :: [CtxElement]
    }
  deriving (Data,Eq,Show,Typeable)




-- | Note Beaming is not captured in parsing, but it is needed
-- in the syntax for output.
--
data CtxElement = Atom    Element
                | Beamed  [CtxElement]
                | Tuplet  TupletSpec    [CtxElement]
  deriving (Data,Eq,Show,Typeable)



-- | LilyPond has a simpler Tuplet spec than ABC which we 
-- expand during parsing.
-- 
data LyTupletSpec = LyTupletSpec Int Int
  deriving (Data,Eq,Show,Typeable)


-- | Note is should be quite easy to add ties (as write-only)
-- to get long notes after beaming.
--
-- See old Neume code. 
--
data Element = NoteElem Note
             | Rest     NoteLength
             | Chord    [Pitch]     NoteLength
             | Graces   [Note]
  deriving (Data,Eq,Show,Typeable)


data Note = Note Pitch NoteLength
  deriving (Data,Eq,Show,Typeable)

data Pitch = Pitch PitchLetter Accidental Octave
  deriving (Data,Eq,Ord,Show,Typeable)


data Accidental = NO_ACCIDENTAL | DBL_FLAT | FLAT | NATURAL | SHARP | DBL_SHARP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

-- | One octave range - just lower
data PitchLetter = CL | DL | EL | FL | GL | AL | BL
  deriving (Data,Enum,Eq,Ord,Show,Typeable)



data Octave = OveDefault
            | OveRaised   Int
            | OveLowered  Int
  deriving (Data,Eq,Ord,Show,Typeable)


data NoteLength = DrnDefault
                | DrnExplicit Duration
  deriving (Data,Eq,Ord,Show,Typeable)



