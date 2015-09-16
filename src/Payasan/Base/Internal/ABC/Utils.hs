{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.Utils
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Helpers for ABC output (pretty printers) and conversion.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABC.Utils
  ( 

  -- * Conversion
    toPitch
  , fromPitch
  , LetterCase(..)
  , decomposePitch
  , recomposePitch
  , fromLetterParts
  , toLetterParts

  , rduration
  , unitLength 

  -- * Output
  , vsep
  , sepList
  , field

  , tupletSpec
  , meter
  , unitNoteLength
  , key
  , mode
  , note
  , rest
  , chord
  , graceForm
  , chordForm
 
  ) where

import Payasan.Base.Internal.ABC.Syntax
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax

import qualified Payasan.Base.Pitch as P
import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJ hiding ( Mode, mode )       -- package: pretty

import Data.Ratio


--------------------------------------------------------------------------------
-- Conversion

-- TODO are pitch conversions Context Free regarding key and alterations?

-- | CU = middle C (octave 4)
--
toPitch :: Pitch -> P.Pitch
toPitch (Pitch a l om) = 
    let (l1,lcase) = toLetterParts l
    in P.Pitch (P.PitchSpelling l1 (toAlteration a)) (toOctaveP lcase om)


fromPitch :: P.Pitch -> Pitch
fromPitch (P.Pitch (P.PitchSpelling l a) o) = 
    Pitch (fromAlteration a) (fromLetterPartsOve l o) (fromOctaveP o)


data LetterCase = LOWER | UPPER
  deriving (Eq,Ord,Show)

-- | General function for decomposing pitch 
-- 
decomposePitch :: Pitch -> (P.PitchSpelling,LetterCase,P.Octave)
decomposePitch (Pitch a l om) = (P.PitchSpelling l1 a1, lc, ove)
  where
    a1          = toAlteration a
    (l1,lc)     = toLetterParts l
    ove         = toOctaveP lc om

recomposePitch :: P.PitchSpelling -> LetterCase -> P.Octave -> Pitch
recomposePitch (P.PitchSpelling l a) lc i = Pitch a1 l1 om
  where
    a1 = fromAlteration a
    l1 = fromLetterParts l lc
    om = fromOctaveP i

toOctaveP :: LetterCase -> Octave -> P.Octave
toOctaveP lc ove = let base = if lc == UPPER then 4 else 5 in step base ove
  where
    step i (OveDefault)   = i
    step i (OveRaised n)  = i + n
    step i (OveLowered n) = i - n


fromOctaveP :: P.Octave -> Octave
fromOctaveP ove | ove < 4       = OveLowered (4 - ove)
                | ove > 5       = OveRaised  (ove - 5)
                | otherwise     = OveDefault

toLetterParts :: PitchLetter -> (P.PitchLetter, LetterCase)
toLetterParts CU   = (P.C, UPPER)
toLetterParts DU   = (P.D, UPPER)
toLetterParts EU   = (P.E, UPPER)
toLetterParts FU   = (P.F, UPPER)
toLetterParts GU   = (P.G, UPPER)
toLetterParts AU   = (P.A, UPPER)
toLetterParts BU   = (P.B, UPPER)
toLetterParts CL   = (P.C, LOWER)
toLetterParts DL   = (P.D, LOWER)
toLetterParts EL   = (P.E, LOWER)
toLetterParts FL   = (P.F, LOWER)
toLetterParts GL   = (P.G, LOWER)
toLetterParts AL   = (P.A, LOWER)
toLetterParts BL   = (P.B, LOWER)

fromLetterParts :: P.PitchLetter -> LetterCase -> PitchLetter
fromLetterParts P.C lc  = if lc == UPPER then CU else CL
fromLetterParts P.D lc  = if lc == UPPER then DU else DL
fromLetterParts P.E lc  = if lc == UPPER then EU else EL
fromLetterParts P.F lc  = if lc == UPPER then FU else FL
fromLetterParts P.G lc  = if lc == UPPER then GU else GL
fromLetterParts P.A lc  = if lc == UPPER then AU else AL
fromLetterParts P.B lc  = if lc == UPPER then BU else BL

fromLetterPartsOve :: P.PitchLetter -> Int -> PitchLetter
fromLetterPartsOve l o 
    | o <= 4    = fromLetterParts l UPPER
    | otherwise = fromLetterParts l LOWER

toAlteration :: Accidental -> P.Alteration
toAlteration NO_ACCIDENTAL      = P.NAT
toAlteration DBL_FLAT           = P.DBL_FLAT
toAlteration FLAT               = P.FLAT
toAlteration NATURAL            = P.NAT
toAlteration SHARP              = P.SHARP
toAlteration DBL_SHARP          = P.DBL_SHARP

fromAlteration :: P.Alteration -> Accidental
fromAlteration P.DBL_FLAT       = DBL_FLAT
fromAlteration P.FLAT           = FLAT
fromAlteration P.NAT            = NO_ACCIDENTAL
fromAlteration P.SHARP          = SHARP
fromAlteration P.DBL_SHARP      = DBL_SHARP




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


--------------------------------------------------------------------------------
-- Pretty printing helpers

vsep :: [Doc] -> Doc
vsep = sepList ($+$)

sepList :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
sepList _  [] = empty
sepList op (x:xs) = step x xs
  where
    step ac []     = ac
    step ac (d:ds) = step (ac `op` d) ds


field :: Char -> Doc -> Doc
field c d = char c <> colon <> d


tupletSpec :: TupletSpec -> Doc
tupletSpec (TupletSpec { tuplet_num   = n
                       , tuplet_time  = t
                       , tuplet_len   = x }) = 
    char '(' <> int n <> char ':' <> int t <> char ':' <> int x



meter :: Meter -> Doc
meter (Meter n d)  = int n <> char '/' <> int d

unitNoteLength :: UnitNoteLength -> Doc
unitNoteLength UNIT_NOTE_4      = text "1/4"
unitNoteLength UNIT_NOTE_8      = text "1/8" 
unitNoteLength UNIT_NOTE_16     = text "1/16"

key :: Key -> Doc
key (Key n m)           = pitchSpelling n <> mode m


mode :: Mode -> Doc
mode MAJOR              = empty
mode MINOR              = text "m"
mode MIXOLYDIAN         = text "Mix"
mode DORIAN             = text "Dor"
mode PHRYGIAN           = text "Phr"
mode LYDIAN             = text "Lyd"
mode LOCRIAN            = text "Loc"


pitchSpelling :: P.PitchSpelling -> Doc
pitchSpelling (P.PitchSpelling l a) = text (show l) <> alt a
  where
    alt P.DBL_FLAT      = text "bb"
    alt P.FLAT          = text "b"
    alt P.NAT           = empty
    alt P.SHARP         = text "#"
    alt P.DBL_SHARP     = text "##"


-- | Print a note. Note that durations in ABC are multipliers of
-- the /unit note length/ rather than absolute values. 
--
note :: ABCNote -> Doc 
note (Note p d) = pitch p <> noteLength d

rest :: NoteLength -> Doc
rest d = char 'z' <> noteLength d


chord :: [Pitch] -> NoteLength -> Doc
chord ps d = chordForm (map pitch ps) <> noteLength d

-- | Print a duration multiplier.
noteLength :: NoteLength -> Doc
noteLength (DNL)      = empty
noteLength (Mult n)   = int n
noteLength (Divd n)   = char '/' <> int n
noteLength (Frac n d) = int n <> char '/' <> int d

pitch :: Pitch -> Doc
pitch (Pitch a l o) = accidental a <> pitchLetter l <> octaveModifier o


octaveModifier:: Octave -> Doc
octaveModifier (OveDefault)     = empty
octaveModifier (OveRaised i)    = text $ replicate i '\''
octaveModifier (OveLowered i)   = text $ replicate i ','



pitchLetter :: PitchLetter -> Doc
pitchLetter CU = char 'C'
pitchLetter DU = char 'D'
pitchLetter EU = char 'E'
pitchLetter FU = char 'F'
pitchLetter GU = char 'G'
pitchLetter AU = char 'A'
pitchLetter BU = char 'B'
pitchLetter CL = char 'c'
pitchLetter DL = char 'd'
pitchLetter EL = char 'e'
pitchLetter FL = char 'f'
pitchLetter GL = char 'g'
pitchLetter AL = char 'a'
pitchLetter BL = char 'b'


accidental :: Accidental -> Doc
accidental NO_ACCIDENTAL = empty
accidental DBL_FLAT      = text "__"
accidental FLAT          = char '_' 
accidental NATURAL       = char '='    
accidental SHARP         = char '^' 
accidental DBL_SHARP     = text "^^"


-- | Chords - notes are printed inside square brackets, e.g.:
-- @ 
--  [c4e4g4]
-- @ 
chordForm :: [Doc] -> Doc
chordForm = brackets . hcat


-- | Grace notes are printed inside braces,
-- e.g:
-- @ 
--  {f2e}
-- @ 
--
-- Not all ABC processors acknowledge duration multipliers within
-- graces.
graceForm :: [Doc] -> Doc
graceForm = braces . hcat
