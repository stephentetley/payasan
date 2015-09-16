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

    LyPhrase
  , LyBar
  , LyNoteGroup
  , LyElement
  , LyNote

  , GenLyPhrase
  , GenLyBar
  , GenLyNoteGroup
  , GenLyElement
  , GenLyNote

  , LyTupletSpec(..)
  , Pitch(..)
  , Accidental(..)
  , PitchLetter(..)
  , Octave(..)
  , NoteLength(..)

  , middle_c

  ) where

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Duration


import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data



--------------------------------------------------------------------------------
-- Syntax


type LyPhrase           = Phrase      Pitch NoteLength
type LyBar              = Bar         Pitch NoteLength
type LyNoteGroup        = NoteGroup   Pitch NoteLength
type LyElement          = Element     Pitch NoteLength
type LyNote             = Note        Pitch NoteLength


type GenLyPhrase pch            = Phrase      pch NoteLength
type GenLyBar pch               = Bar         pch NoteLength
type GenLyNoteGroup pch         = NoteGroup   pch NoteLength
type GenLyElement pch           = Element     pch NoteLength
type GenLyNote pch              = Note        pch NoteLength


-- | LilyPond has a simpler Tuplet spec than ABC which we 
-- expand during parsing.
-- 
data LyTupletSpec = LyTupletSpec Int Int
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



middle_c :: Pitch
middle_c = Pitch CL NO_ACCIDENTAL (OveRaised 1)



--------------------------------------------------------------------------------
-- Pretty instances are for debugging and do not correspond 
-- to valid LilyPond

instance Pretty Pitch where 
  pPrint (Pitch l a om)         = pPrint l <> pPrint a <> pPrint om

instance Pretty Accidental where
  pPrint NO_ACCIDENTAL          = empty
  pPrint DBL_FLAT               = text "eses"
  pPrint FLAT                   = text "es"
  pPrint NATURAL                = text "nat"
  pPrint SHARP                  = text "is"
  pPrint DBL_SHARP              = text "isis"

instance Pretty PitchLetter where
  pPrint CL                     = char 'c'
  pPrint DL                     = char 'd'
  pPrint EL                     = char 'e'
  pPrint FL                     = char 'f'
  pPrint GL                     = char 'g'
  pPrint AL                     = char 'a'
  pPrint BL                     = char 'b'

instance Pretty Octave where
  pPrint (OveDefault)           = empty
  pPrint (OveRaised i)          = text (replicate i '\'')
  pPrint (OveLowered i)         = text (replicate i ',')


instance Pretty NoteLength where
  pPrint (DrnDefault)           = char '.'
  pPrint (DrnExplicit d)        = pPrint d

