{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.Syntax
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

module Payasan.Base.Internal.ABC.Syntax
  ( 

    ABCPhrase
  , ABCBar
  , ABCNoteGroup
  , ABCElement
  , ABCNote


  , ABCPitch(..)
  , Accidental(..)
  , PitchLetter(..)
  , Octave(..)
  , ABCNoteLength(..)

  , middle_c

  ) where

import Payasan.Base.Internal.BeamSyntax

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data

--------------------------------------------------------------------------------
-- Syntax

-- | ABC is not annotated

type ABCPhrase          = Phrase      ABCPitch ABCNoteLength ()
type ABCBar             = Bar         ABCPitch ABCNoteLength ()
type ABCNoteGroup       = NoteGroup   ABCPitch ABCNoteLength ()
type ABCElement         = Element     ABCPitch ABCNoteLength ()
type ABCNote            = Note        ABCPitch ABCNoteLength


-- Just give Picth and NoteLength ABC- prefix as they are 
-- likely to otherwise cause confusion in debug output and 
-- conversion code.

data ABCPitch = ABCPitch Accidental PitchLetter Octave
  deriving (Data,Eq,Ord,Show,Typeable)


data Accidental = NO_ACCIDENTAL | DBL_FLAT | FLAT | NATURAL | SHARP | DBL_SHARP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

-- | Two octave range - upper and lower
data PitchLetter = CU | DU | EU | FU | GU | AU | BU 
                 | CL | DL | EL | FL | GL | AL | BL 
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


data Octave = OveDefault
            | OveRaised   Int
            | OveLowered  Int
  deriving (Data,Eq,Ord,Show,Typeable)



-- | does this need @Frac Int Int@ ?
--
data ABCNoteLength = DNL
                   | Mult Int
                   | Divd Int
                   | Frac Int Int
  deriving (Data,Eq,Ord,Show,Typeable)



middle_c :: ABCPitch
middle_c = ABCPitch NO_ACCIDENTAL CU OveDefault


--------------------------------------------------------------------------------
-- Pretty instances are for debugging and do not correspond 
-- to valid LilyPond

instance Pretty ABCPitch where 
  pPrint (ABCPitch a l om)      = pPrint a <> pPrint l <> pPrint om

instance Pretty Accidental where
  pPrint NO_ACCIDENTAL          = empty
  pPrint DBL_FLAT               = text "__"
  pPrint FLAT                   = text "_"
  pPrint NATURAL                = text "="
  pPrint SHARP                  = text "^"
  pPrint DBL_SHARP              = text "^^"

instance Pretty PitchLetter where
  pPrint CU                     = char 'C'
  pPrint DU                     = char 'D'
  pPrint EU                     = char 'E'
  pPrint FU                     = char 'F'
  pPrint GU                     = char 'G'
  pPrint AU                     = char 'A'
  pPrint BU                     = char 'B'
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


instance Pretty ABCNoteLength where
  pPrint (DNL)                  = empty
  pPrint (Mult n)               = int n <> text "/1"
  pPrint (Divd d)               = text "1/"<> int d
  pPrint (Frac n d)             = int n <> char '/' <> int d



