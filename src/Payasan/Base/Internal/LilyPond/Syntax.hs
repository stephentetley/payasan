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
  , LyPitch(..)
  , Accidental(..)
  , PitchLetter(..)
  , Octave(..)
  , LyNoteLength(..)

  , middle_c

  ) where

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Duration


import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data



--------------------------------------------------------------------------------
-- Syntax


type LyPhrase anno              = Phrase      LyPitch LyNoteLength anno
type LyBar anno                 = Bar         LyPitch LyNoteLength anno
type LyNoteGroup anno           = NoteGroup   LyPitch LyNoteLength anno
type LyElement anno             = Element     LyPitch LyNoteLength anno
type LyNote                     = Note        LyPitch LyNoteLength


type GenLyPhrase pch anno       = Phrase      pch LyNoteLength anno
type GenLyBar pch anno          = Bar         pch LyNoteLength anno
type GenLyNoteGroup pch anno    = NoteGroup   pch LyNoteLength anno
type GenLyElement pch anno      = Element     pch LyNoteLength anno
type GenLyNote pch              = Note        pch LyNoteLength


-- | LilyPond has a simpler Tuplet spec than ABC which we 
-- expand during parsing.
-- 
data LyTupletSpec = LyTupletSpec Int Int
  deriving (Data,Eq,Show,Typeable)


data LyPitch = LyPitch PitchLetter Accidental Octave
  deriving (Data,Eq,Ord,Show,Typeable)


data Accidental = NO_ACCIDENTAL | DBL_FLAT | FLAT | NATURAL | SHARP | DBL_SHARP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

-- | One octave range - just lower
data PitchLetter = C | D | E | F | G | A | B
  deriving (Data,Enum,Eq,Ord,Show,Typeable)



data Octave = OveDefault
            | OveRaised   Int
            | OveLowered  Int
  deriving (Data,Eq,Ord,Show,Typeable)


data LyNoteLength = DrnDefault
                  | DrnExplicit Duration
  deriving (Data,Eq,Ord,Show,Typeable)



middle_c :: LyPitch
middle_c = LyPitch C NO_ACCIDENTAL (OveRaised 1)



--------------------------------------------------------------------------------
-- Pretty instances are for debugging and do not correspond 
-- to valid LilyPond

instance Pretty LyPitch where 
  pPrint (LyPitch l a om)       = pPrint l <> pPrint a <> pPrint om

instance Pretty Accidental where
  pPrint NO_ACCIDENTAL          = empty
  pPrint DBL_FLAT               = text "eses"
  pPrint FLAT                   = text "es"
  pPrint NATURAL                = text "nat"
  pPrint SHARP                  = text "is"
  pPrint DBL_SHARP              = text "isis"

instance Pretty PitchLetter where
  pPrint C                      = char 'c'
  pPrint D                      = char 'd'
  pPrint E                      = char 'e'
  pPrint F                      = char 'f'
  pPrint G                      = char 'g'
  pPrint A                      = char 'a'
  pPrint B                      = char 'b'

instance Pretty Octave where
  pPrint (OveDefault)           = empty
  pPrint (OveRaised i)          = text (replicate i '\'')
  pPrint (OveLowered i)         = text (replicate i ',')


instance Pretty LyNoteLength where
  pPrint (DrnDefault)           = char '.'
  pPrint (DrnExplicit d)        = pPrint d

