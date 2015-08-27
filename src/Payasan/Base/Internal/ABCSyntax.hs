{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABCSyntax
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

module Payasan.Base.Internal.ABCSyntax
  ( 
    module Payasan.Base.Internal.CommonSyntax

  , ABCPhrase(..)
  , Bar(..)
  , CtxElement(..)
  , Element(..)
  , Note(..)
  , Accidental(..)
  , PitchLetter(..)
  , Octave(..)
  , Pitch(..)
  , NoteLength(..)


  , elementSize
  , rduration
  , unitLength 

  ) where

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Duration

import Data.Data
import Data.Ratio

--------------------------------------------------------------------------------
-- Syntax

-- This is a /concrete syntax/ roughly equal to what is parsed
-- An abstract syntax should prefix each bar with 
-- (key_sig, time_sig) then they can be freely combined.


-- | ABC is read from the quasiquoter with an initial
-- key sig and metrical information, but when it is used as 
-- an output format it might have diffrent key sigs and 
-- meter info from diferrent fragments.
--
-- The use the ABC prefix here because the type may be exposed 
-- in user code and we don\'t want a name clash.
--
data ABCPhrase = ABCPhrase { phrase_bars :: [Bar] }
  deriving (Data,Eq,Show,Typeable)




-- | Note Beaming is not captured in parsing.
--
data Bar = Bar 
    { render_info       :: RenderInfo
    , bar_elements      :: [CtxElement]
    }
  deriving (Data,Eq,Show,Typeable)




-- | Note Beaming is not captured in parsing.
--
data CtxElement = Atom    Element
                | Beamed  [CtxElement]
                | Tuplet  TupletSpec    [CtxElement]
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

data Pitch = Pitch Accidental PitchLetter Octave
  deriving (Data,Eq,Ord,Show,Typeable)


data Accidental = NO_ACCIDENTAL | DBL_FLAT | FLAT | NATURAL | SHARP | DBL_SHARP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

-- | Two octave range - upper and lower
data PitchLetter = CU | DU | EU | FU | GU | AU | BU 
                 | CL | DL | EL | FL | GL | AL | BL 
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


data Octave = OveDefault
            | OvePos Int
            | OveNeg Int
  deriving (Data,Eq,Ord,Show,Typeable)



-- | does this need @Frac Int Int@ ?
--
data NoteLength = DNL
                | Mult Int
                | Divd Int
                | Frac Int Int
  deriving (Data,Eq,Ord,Show,Typeable)



elementSize :: CtxElement -> Int
elementSize (Tuplet spec _) = tuplet_len spec
elementSize (Beamed xs)     = sum $ map elementSize xs
elementSize _               = 1



-- UnitNoteLength = UNIT_NOTE_8 | UNIT_NOTE_16

rduration :: UnitNoteLength -> NoteLength -> RDuration
rduration unl (DNL)      = unitLength unl
rduration unl (Mult i)   = let r = fromIntegral i in r * unitLength unl
rduration unl (Divd i)   = let r = fromIntegral i in (unitLength unl) / r
rduration unl (Frac n d) = 
    let nr = fromIntegral n; nd = fromIntegral d in (unitLength unl) * (nr%nd)


unitLength :: UnitNoteLength -> RDuration
unitLength UNIT_NOTE_4  = 1%4
unitLength UNIT_NOTE_8  = 1%8
unitLength UNIT_NOTE_16 = 1%16


