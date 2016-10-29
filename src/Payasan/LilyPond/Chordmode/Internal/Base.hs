{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Chordmode.Internal.Base
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Chordmode for LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Chordmode.Internal.Base
  ( 
   
    LyChordPart
  , OutChordPart
  , LyChordRoot

  , StdChordPart

  , Chord(..)
  , LyChord(..)
  , ChordSuffix(..)
  , ChordModifier(..)
  , Steps(..)
  , Step(..)
  , Alt(..)

  ) where

import qualified Payasan.PSC.Repr.IRBeam.Syntax             as BEAM
import qualified Payasan.Score.Elementary.Internal.Syntax   as ELEM

import Payasan.Base.Internal.LilyPond.Syntax


import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data


-- LilyPond is input as a Elementary note list, but output as
-- beam syntax (with bars).

type LyChordPart        = ELEM.Part LyChordRoot LyNoteLength ChordSuffix
type OutChordPart       = BEAM.Part LyChordRoot LyNoteLength ChordSuffix

type LyChordRoot        = LyPitch

type StdChordPart       = ELEM.Part Chord   Duration      ()


-- Design note - Chord seems too fundamental and LyChord
-- too transitory to warrant a parametric datatype.

data Chord = Chord 
    { chord_root    :: Pitch
    , chord_suffix  :: ChordSuffix
    }
  deriving (Data,Eq,Ord,Show,Typeable)


data LyChord = LyChord LyPitch ChordSuffix
  deriving (Data,Eq,Ord,Show,Typeable)


-- | Use a more limited subset than LilyPond supports - 
-- either a named modifier or a set of steps.
--
-- @empty@ is e.g. default maj5 is @SuffixModifier NO_MOD@
--
data ChordSuffix = NamedModifier ChordModifier 
                 | ChordSteps    Steps
  deriving (Data,Eq,Ord,Show,Typeable)

data Steps = Steps 
    { additions     :: [Step]
    , removals      :: [Step]
    }
  deriving (Data,Eq,Ord,Show,Typeable)

-- | LilyPond allows arbitrary qualities (e.g. min4) but their 
-- interpretation seems opaque at least for major chords. 
-- So we limit premissible input just to simple cases.
--
-- Notation.pdf - Appendix A2 has a list of common chord 
-- modifiers (some have symbols, some built with steps).
--
data ChordModifier = 
      MAJ5      | MAJ6    | MAJ7    | MAJ9    | MAJ11   | MAJ13
    | MIN5      | MIN6    | MIN7    | MIN9    | MIN11   | MIN13
    | DIM5      | DIM7
    | AUG5      | AUG7
    | DOM7      | DOM9    | DOM11   | DOM13
    | MM7
    | SUS       | SUS2    | SUS4
    | NO_MOD
  deriving (Data,Eq,Ord,Show,Typeable)

data Step = Step Int Alt
  deriving (Data,Eq,Ord,Show,Typeable)

data Alt = NVE | NO_ALT | PVE
  deriving (Data,Eq,Ord,Show,Typeable)


-- Probably avoid inversions


instance Pretty Chord where
  pPrint (Chord p suffix)       = pPrint p <> char ':' <> pPrint suffix

instance Pretty ChordSuffix where
  pPrint (NamedModifier cmod)   = pPrint cmod
  pPrint (ChordSteps steps)     = pPrint steps



instance Pretty ChordModifier where
  pPrint MAJ5           = text "M5"
  pPrint MAJ6           = text "M6"
  pPrint MAJ7           = text "M7"
  pPrint MAJ9           = text "M9"
  pPrint MAJ11          = text "M11"
  pPrint MAJ13          = text "M13"
  pPrint MIN5           = text "m5"
  pPrint MIN6           = text "m6"
  pPrint MIN7           = text "m7"
  pPrint MIN9           = text "m9"
  pPrint MIN11          = text "m11"
  pPrint MIN13          = text "m13"
  pPrint DIM5           = text "dim5"
  pPrint DIM7           = text "dim7"
  pPrint AUG5           = text "aug5"
  pPrint AUG7           = text "aug7"
  pPrint DOM7           = text "dom8"
  pPrint DOM9           = text "dom9"
  pPrint DOM11          = text "dom11"
  pPrint DOM13          = text "dom13"
  pPrint MM7            = text "Mm7"
  pPrint SUS            = text "sus"
  pPrint SUS2           = text "sus2"
  pPrint SUS4           = text "sus4"
  pPrint NO_MOD         = empty


instance Pretty Steps where
  pPrint (Steps { additions = adds, removals = subs }) =
      dotSep (map pPrint adds) <> mkSubs subs
    where
      mkSubs [] = empty
      mkSubs xs = char '^' <> dotSep (map pPrint xs)

instance Pretty Step where
  pPrint (Step i alt)   = int i <> pPrint alt

instance Pretty Alt where
  pPrint NVE            = char '-'
  pPrint NO_ALT         = empty
  pPrint PVE            = char '+'

dotSep :: [Doc] -> Doc
dotSep []     = empty
dotSep (x:xs) = step x xs
  where
    step d []       = d
    step d (a:as)   = d <> char '.' <> step a as