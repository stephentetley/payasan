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
  , OctaveModifier(..)
  , ABCNoteLength(..)

  , middle_c

  , toPitch
  , fromPitch

  , toDuration
  , fromDuration

  ) where

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.Scale

import qualified Payasan.Base.Pitch as PCH
import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data
import Data.Ratio

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

data ABCPitch = ABCPitch Accidental PitchLetter OctaveModifier
  deriving (Data,Eq,Ord,Show,Typeable)


data Accidental = NO_ACCIDENTAL | DBL_FLAT | FLAT | NATURAL | SHARP | DBL_SHARP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

-- | Two octave range - upper and lower
data PitchLetter = CU | DU | EU | FU | GU | AU | BU 
                 | CL | DL | EL | FL | GL | AL | BL 
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


data OctaveModifier = OveDefault
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


-- | If pitch is NO_ACCIDENTAL, alteration needs looking up in the scale
--
toPitch :: Scale -> ABCPitch -> PCH.Pitch
toPitch sc (ABCPitch a l om) = PCH.Pitch (PCH.PitchName l1 a1) ove
  where
    (l1,o1) = decomposePitchLetter l
    ove     = modifyOctave o1 om
    a1      = case findAlteration l1 sc of
                Nothing -> toAlteration a
                Just alt -> alt


modifyOctave :: PCH.Octave -> OctaveModifier -> PCH.Octave
modifyOctave i (OveDefault)   = i
modifyOctave i (OveRaised n)  = i + n
modifyOctave i (OveLowered n) = i - n


decomposePitchLetter :: PitchLetter -> (PCH.PitchLetter, PCH.Octave)
decomposePitchLetter CU         = (PCH.C, 4)
decomposePitchLetter DU         = (PCH.D, 4)
decomposePitchLetter EU         = (PCH.E, 4)
decomposePitchLetter FU         = (PCH.F, 4)
decomposePitchLetter GU         = (PCH.G, 4)
decomposePitchLetter AU         = (PCH.A, 4)
decomposePitchLetter BU         = (PCH.B, 4)
decomposePitchLetter CL         = (PCH.C, 5)
decomposePitchLetter DL         = (PCH.D, 5)
decomposePitchLetter EL         = (PCH.E, 5)
decomposePitchLetter FL         = (PCH.F, 5)
decomposePitchLetter GL         = (PCH.G, 5)
decomposePitchLetter AL         = (PCH.A, 5)
decomposePitchLetter BL         = (PCH.B, 5)



fromPitch :: Scale -> PCH.Pitch -> ABCPitch
fromPitch sc p@(PCH.Pitch (PCH.PitchName l a) o) = ABCPitch a1 l1 om 
  where
    l1 = calcPitchLetter l o
    a1 = if isScaleTone p sc then NO_ACCIDENTAL else fromAlteration a
    om = fromOctave o

calcPitchLetter :: PCH.PitchLetter -> PCH.Octave -> PitchLetter
calcPitchLetter PCH.C o         = if o >= 5 then CU else CL
calcPitchLetter PCH.D o         = if o >= 5 then DU else DL
calcPitchLetter PCH.E o         = if o >= 5 then EU else EL
calcPitchLetter PCH.F o         = if o >= 5 then FU else FL
calcPitchLetter PCH.G o         = if o >= 5 then GU else GL
calcPitchLetter PCH.A o         = if o >= 5 then AU else AL
calcPitchLetter PCH.B o         = if o >= 5 then BU else BL

fromOctave :: PCH.Octave -> OctaveModifier
fromOctave ove | ove < 4       = OveLowered (4 - ove)
               | ove > 5       = OveRaised  (ove - 5)
               | otherwise     = OveDefault



toAlteration :: Accidental -> PCH.Alteration
toAlteration NO_ACCIDENTAL      = PCH.NAT
toAlteration DBL_FLAT           = PCH.DBL_FLAT
toAlteration FLAT               = PCH.FLAT
toAlteration NATURAL            = PCH.NAT
toAlteration SHARP              = PCH.SHARP
toAlteration DBL_SHARP          = PCH.DBL_SHARP

fromAlteration :: PCH.Alteration -> Accidental
fromAlteration PCH.DBL_FLAT     = DBL_FLAT
fromAlteration PCH.FLAT         = FLAT
fromAlteration PCH.NAT          = NATURAL
fromAlteration PCH.SHARP        = SHARP
fromAlteration PCH.DBL_SHARP    = DBL_SHARP


toDuration :: UnitNoteLength -> ABCNoteLength -> Duration
toDuration unl d = 
    case rationalToDuration $ rduration unl d of
      Nothing -> d_longa
      Just ans -> ans


rduration :: UnitNoteLength -> ABCNoteLength -> RDuration
rduration unl (DNL)      = unitLength unl
rduration unl (Mult i)   = let r = fromIntegral i in r * unitLength unl
rduration unl (Divd i)   = let r = fromIntegral i in (unitLength unl) / r
rduration unl (Frac n d) = 
    let nr = fromIntegral n; nd = fromIntegral d in (unitLength unl) * (nr%nd)


unitLength :: UnitNoteLength -> RDuration
unitLength UNIT_NOTE_4  = 1%4
unitLength UNIT_NOTE_8  = 1%8
unitLength UNIT_NOTE_16 = 1%16


fromDuration :: UnitNoteLength -> Duration -> ABCNoteLength
fromDuration unl nd = 
    (fn . fork numerator denominator) $ (durationSize nd) / unitLength unl
  where  
    fork f g a = (f a, g a)
    fn (1,1)   = DNL
    fn (1,dn)  = Divd (fromIntegral dn)
    fn (nm,1)  = Mult (fromIntegral nm)
    fn (nm,dn) = Frac (fromIntegral nm) (fromIntegral dn)

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

instance Pretty OctaveModifier where
  pPrint (OveDefault)           = empty
  pPrint (OveRaised i)          = text (replicate i '\'')
  pPrint (OveLowered i)         = text (replicate i ',')


instance Pretty ABCNoteLength where
  pPrint (DNL)                  = empty
  pPrint (Mult n)               = int n <> text "/1"
  pPrint (Divd d)               = text "1/"<> int d
  pPrint (Frac n d)             = int n <> char '/' <> int d



