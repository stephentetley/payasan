{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Base.SyntaxCommon
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common syntax elements
--
--------------------------------------------------------------------------------

module Payasan.PSC.Base.SyntaxCommon
  ( 

    TyDoc(..)
  , TyText(..)
  

  , SectionInfo(..)
  , UnitNoteLength(..)

  , Tie(..)
  , Anno(..)
  , AnnoDU(..)

  , default_section_info

  , sectionKeyWithDefault

  , barRatDuration

  , MeterPattern
  , TupletSpec(..)
  , tupletUnitRatDuration 


  -- * Keys
  , c_maj

  ) where

import Payasan.Base.Basis
import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Scale


import Text.PrettyPrint.HughesPJ hiding (Mode)               -- package: pretty


import qualified Data.Text as TEXT

import Data.Data
import Data.Ratio

data TyDoc a = TyDoc { extractDoc :: Doc }


data TyText a = TyText { extractText :: TEXT.Text }






--------------------------------------------------------------------------------
-- Score syntax elements


-- Note - don\'t store LilyPond Absolute / Relative pitch
-- at bar level. This a a global property as we must render
-- a notelist in one mode only.
--
-- Some data is usefully "render info" but should be specified by
-- library writer not by the end user.
--
-- Potentially there is a relationship between a user exposed 
-- config and a larger internal one like Parsec\'s LanguageDef 
-- and TokenParser
--
-- CORRECTION - AbsPitch / Relative Pitch is actually a 
-- voice (part) level property.
--


  

-- | Note - @SectionInfo@ is stored as a header to a Section in
-- External syntax.
-- 
-- This prevents (easy) concat of Sections - Sections would need
-- identical headers, with differing headers Sections cannot 
-- concat
--
-- SectionInfo should avoid backend specific info.
--
data SectionInfo = SectionInfo
    { section_key               :: Maybe Key
    , section_meter             :: !Meter
    , section_meter_pattern     :: !MeterPattern
    , section_unit_note_len     :: !UnitNoteLength
    }
  deriving (Data,Eq,Show,Typeable)





-- TODO - nice to span bars with meter pattern 
-- e.g. for 2-bar patterns as in Latin music.
--
type MeterPattern = [RatDuration]

data UnitNoteLength = UNIT_NOTE_4 | UNIT_NOTE_8 | UNIT_NOTE_16
  deriving (Data,Enum,Eq,Ord,Show,Typeable)




default_section_info :: SectionInfo
default_section_info = SectionInfo 
    { section_key               = Just $ c_maj
    , section_meter             = Metered $ TimeSig 4 4 
    , section_meter_pattern     = [1%2,1%2]
    , section_unit_note_len     = UNIT_NOTE_8
    }


sectionKeyWithDefault :: Key -> SectionInfo -> Key
sectionKeyWithDefault dflt = maybe dflt id <$> section_key


data Tie = TIE | NO_TIE
  deriving (Data,Enum,Eq,Ord,Show,Typeable)



class Anno a where anno :: a -> Doc

instance Anno () where anno = const empty



-- DEPRECATED
-- The idea behind this wasn't so well founded. Defs should
-- probably go somehow directly into the score template.
--
data AnnoDU a = AnnoDU 
    { anno_defs :: Doc                  -- multiple annos
    , anno_use  :: a -> Doc 
    }





barRatDuration :: TimeSig -> RatDuration
barRatDuration (TimeSig n d) = (fromIntegral n) * fn d
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


tupletUnitRatDuration :: TupletSpec -> RatDuration -> RatDuration 
tupletUnitRatDuration (TupletSpec { tuplet_time_mult = m }) unitd = 
    unitd * realToFrac m

  

c_maj :: Key
c_maj = Key (PitchName C NAT) MAJOR
