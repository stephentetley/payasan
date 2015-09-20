{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Chordmode.Internal.Base
-- Copyright   :  (c Stephen Tetley 2015
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
   
    LyChordPhrase
  , OutChordPhrase
  , LyChordRoot

  , StdChordPhrase

  , Chord(..)
  , LyChord(..)
  , ChordSuffix(..)
  , ChordModifier(..)
  , Steps(..)
  , Step(..)
  , Alt(..)

  ) where

import qualified Payasan.Base.Monophonic.Internal.Syntax    as MONO

import qualified Payasan.Base.Internal.LilyPond.Syntax      as LY
import qualified Payasan.Base.Internal.BeamSyntax           as BEAM


import Payasan.Base.Duration
import Payasan.Base.Pitch

import Data.Data


-- LilyPond is input as a Monophonic note list, but output as
-- beam syntax (with bars).

type LyChordPhrase       = MONO.Phrase LyChordRoot LY.NoteLength ChordSuffix
type OutChordPhrase      = BEAM.Phrase LyChordRoot LY.NoteLength ChordSuffix

type LyChordRoot         = LY.Pitch

type StdChordPhrase      = MONO.Phrase Chord   Duration      ()


-- Design note - Chord seems too fundamental and LyChord
-- too transitory to warrant a parametric datatype.

data Chord = Chord 
    { chord_root    :: Pitch
    , chord_suffix  :: ChordSuffix
    }
  deriving (Data,Eq,Ord,Show,Typeable)


data LyChord = LyChord LY.Pitch ChordSuffix
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


