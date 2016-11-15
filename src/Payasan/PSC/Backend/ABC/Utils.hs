{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Backend.ABC.Utils
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Helpers for ABC output (pretty printers).
--
--------------------------------------------------------------------------------

module Payasan.PSC.Backend.ABC.Utils
  ( 

  -- * Output
    vsep
  , sepList
  , field
  , midtuneField

  , tupletSpec
  , meter
  , time
  , unitNoteLength
  , key
  , clef
  , mode
  , note
  , rest
  , spacer
  , chord
  , graceForm
  , chordForm
 
  ) where

import Payasan.PSC.Repr.External.ABCAliases
import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Base.ABCCommon
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Basis
import qualified Payasan.Base.Pitch as P
import Payasan.Base.Scale

import Text.PrettyPrint.HughesPJ hiding ( Mode, mode )       -- package: pretty





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

midtuneField :: Char -> Doc -> Doc
midtuneField c d = char '[' <> field c d <> char ']'


tupletSpec :: TupletSpec -> Doc
tupletSpec (TupletSpec { tuplet_num       = n
                       , tuplet_time_mult = t
                       , tuplet_len       = x }) = 
    char '(' <> int n <> char ':' <> int t <> char ':' <> int x


meter :: Meter -> Doc
meter (Unmetered)       = text "none"
meter (TimeSig t)       = time t

time :: TimeRatio -> Doc
time (TimeRatio n d)    = int n <> char '/' <> int d

unitNoteLength :: UnitNoteLength -> Doc
unitNoteLength UNIT_NOTE_4      = text "1/4"
unitNoteLength UNIT_NOTE_8      = text "1/8" 
unitNoteLength UNIT_NOTE_16     = text "1/16"

key :: Key -> Doc
key (Key n m)           = pitchName n <> mode m

clef :: Clef -> Doc
clef TREBLE             = text "clef=treble"
clef BASS               = text "clef=bass"


mode :: Mode -> Doc
mode MAJOR              = empty
mode MINOR              = text "m"
mode MIXOLYDIAN         = text "Mix"
mode DORIAN             = text "Dor"
mode PHRYGIAN           = text "Phr"
mode LYDIAN             = text "Lyd"
mode LOCRIAN            = text "Loc"


pitchName :: P.PitchName -> Doc
pitchName (P.PitchName l a) = text (show l) <> alt a
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

rest :: ABCNoteLength -> Doc
rest d = char 'z' <> noteLength d


-- | As @x@...
--
spacer :: ABCNoteLength -> Doc
spacer d = char 'x' <> noteLength d


chord :: [ABCPitch] -> ABCNoteLength -> Doc
chord ps d = chordForm (map pitch ps) <> noteLength d

-- | Print a duration multiplier.
noteLength :: ABCNoteLength -> Doc
noteLength (DNL)      = empty
noteLength (Mult n)   = int n
noteLength (Divd n)   = char '/' <> int n
noteLength (Frac n d) = int n <> char '/' <> int d

pitch :: ABCPitch -> Doc
pitch (ABCPitch a l o) = accidental a <> pitchLetter l <> octaveModifier o


octaveModifier:: OctaveModifier -> Doc
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
