{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.CommonSyntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common syntax elements
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.CommonSyntax
  ( 

    ScoreInfo(..)
  , StaffInfo(..)
  , Clef(..)

  , default_score_info
  , default_staff_info 

  , LocalContextInfo(..)
  , UnitNoteLength(..)

  , Tie(..)
  , Anno(..)
  , AnnoDU(..)


  , Markup
  , markup
  , no_markup
  , renderMarkup

  , Key(..)
  , Mode(..)
  , Meter(..)

  , default_local_info

  , barLength

  , MeterPattern
  , TupletSpec(..)
  , tupletUnitRDuration 


  -- * Keys
  , c_maj

  ) where

import Payasan.Base.Internal.Base

import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJ hiding (Mode)               -- package: pretty

import Data.Data
import Data.Ratio




-- | Note - don\'t store LilyPond Absolute / Relative pitch
-- at bar level. This a a global property as we must render
-- in one mode only.

-- Some data is usefully render info but should be specified by
-- library writer not by the end user, e.g. global_do_beaming
--
-- Potentially there is a relationship between a user exposed 
-- config and a larger internal one like Parsec\'s LanguageDef 
-- and TokenParser
--
-- CORRECTION - AbsPitch / Relative Pitch is actually a voice level
-- property.
--


data ScoreInfo = ScoreInfo
    { score_title               :: !String
    , score_ly_version          :: !String
    }
  deriving (Data,Eq,Show,Typeable)


data StaffInfo = StaffInfo 
    { staff_clef                :: !Clef
    }
  deriving (Data,Eq,Show,Typeable)


data Clef = TREBLE | BASS
  deriving (Data,Eq,Show,Typeable)


default_score_info :: ScoreInfo
default_score_info = ScoreInfo
    { score_title               = ""
    , score_ly_version          = "2.18.2"
    }


default_staff_info :: StaffInfo
default_staff_info = StaffInfo 
    { staff_clef                = TREBLE
    }


-- | Note - @LocalRenderInfo@ is stored as a header to a Bar in
-- Beam and Main syntax.
-- 
-- This allows concatenating bars together (and tempo and key 
-- changes). Generally outputting must be sensitive to changes 
-- to LocalRenderInfo as a new bar is printed.
-- 
-- Mono syntax is more suitable to transformation, hence we put 
-- @LocalRenderInfo@ at the start of the phrase
--

data LocalContextInfo = LocalContextInfo
    { local_key                 :: !Key
    , local_meter               :: !Meter
    , local_meter_patn          :: !MeterPattern
    , local_unit_note_len       :: !UnitNoteLength
    , local_bpm                 :: !BPM
    }
  deriving (Data,Eq,Show,Typeable)


data Tie = TIE | NO_TIE
  deriving (Data,Enum,Eq,Ord,Show,Typeable)



class Anno a where anno :: a -> Doc

instance Anno () where anno = const empty


data AnnoDU a = AnnoDU { defs :: Doc, use :: a -> Doc }


-- | Unfortunately Markup has to be a String internally (not a 
-- Doc) so it can have a Data instance.
-- 
data Markup = Markup !String
  deriving (Data,Eq,Show,Typeable)


instance Monoid Markup where
  mempty = Markup ""
  Markup a `mappend` Markup b 
      | null a      = Markup b
      | null b      = Markup a
      | otherwise   = Markup $ a ++ (' ': b)                          


markup :: Doc -> Markup
markup d = Markup $ renderStyle (style {lineLength=500}) d

no_markup :: Markup 
no_markup = Markup ""


-- | TODO - markup can be above @^@, below @_@ or default @-@.
--
-- See 5.4.2 Direction and placement
-- 
-- Does putting placement inside Markup conflict with concat or
-- can each markup have its own placement?
--
renderMarkup :: Markup -> Doc
renderMarkup (Markup s) 
    | null s    = empty 
    | otherwise = char '^' <> text "\\markup" <+> text s




data Key = Key !PitchName !Mode
  deriving (Data,Eq,Ord,Show,Typeable)


data Mode = MAJOR | MINOR | MIXOLYDIAN | DORIAN | PHRYGIAN | LYDIAN | LOCRIAN
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

-- | CommonTime = 4/4
--   CutTime = 2/4
--
-- TODO - add free metered.
--
data Meter = Meter Int Int
  deriving (Data,Eq,Ord,Show,Typeable)

-- TODO - span bars with meter pattern (e.g. for 2-bar patterns
-- as in South American music).
--
type MeterPattern = [RDuration]

data UnitNoteLength = UNIT_NOTE_4 | UNIT_NOTE_8 | UNIT_NOTE_16
  deriving (Data,Enum,Eq,Ord,Show,Typeable)







default_local_info :: LocalContextInfo
default_local_info = LocalContextInfo 
    { local_key                 = c_maj
    , local_meter               = Meter 4 4 
    , local_meter_patn          = [1%2,1%2]
    , local_unit_note_len       = UNIT_NOTE_8
    , local_bpm                 = 120
    }






barLength :: Meter -> RDuration
barLength (Meter n d) = (fromIntegral n) * fn d
  where
    fn i = 1 % fromIntegral i


-- | @tuplet_time_mult@ is the multipler of the note 
-- length (ideally all notes _should_ be the same length).
--
data TupletSpec = TupletSpec
     { tuplet_num       :: !Int
     , tuplet_time_mult :: !Int
     , tuplet_len       :: !Int
     }
  deriving (Data,Eq,Show,Typeable)


tupletUnitRDuration :: TupletSpec -> RDuration -> RDuration 
tupletUnitRDuration (TupletSpec { tuplet_time_mult = m }) unitd = 
    unitd * realToFrac m




  

c_maj :: Key
c_maj = Key (PitchName C NAT) MAJOR
