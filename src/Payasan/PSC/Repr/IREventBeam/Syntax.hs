{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventBeam.Syntax
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Eventlist syntax that retains some structure (sections, bars). 
-- Intended for low level metrical and articulation transforms.
-- E.g. quantization.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IREventBeam.Syntax
  ( 
    Part(..)
  , Section(..)
  , Bar(..)
  , Event(..)

  ) where

import Data.Data

-- Design note
-- The point of this eventlist representation is to support 
-- temporal and expressive rendering (e.g quantization, 
-- accentation) and potentially crescendos etc. 
--
-- It is expected that these transformations will require some 
-- metrical "addressing" as to when they are activated, hence 
-- we retain division into sections and bars.
--


-- This representation should support onset changing 
-- transformations, but disallow duration changing 
-- transformation, hence duration is hidden in the event body. 
--
-- Duration for an event does not match metrical length of a 
-- note in a score. For an event, duration might be e.g. a 
-- fixed length for all notes (e.g. marimba notes that all decay 
-- for the same length). Musically, it is the successive onsets 
-- of marimba notes that are variable to match a rhythm.
--
data Part ot evt = Part { part_sections :: [Section ot evt] }
  deriving (Data,Eq,Show,Typeable)

-- | We keep section in this syntax. Having named sections is
-- expected to allow transformations limited to a specific region.
--
data Section ot evt = Section
    { section_name      :: !String
    , section_onset     :: !ot
    , section_bars      :: [Bar ot evt]
    }
  deriving (Data,Eq,Show,Typeable)

  
data Bar ot evt = Bar
    { bar_onset         :: !ot
    , bar_events        :: [Event ot evt]
    }
  deriving (Data,Eq,Show,Typeable)

-- TODO - ideally duration should be abstract and not visible to 
-- users. What ramifications does this have for Csound / MIDI 
-- renderers? And when must we perform translations from Seconds
-- (e.g. to MIDI ticks) if they are needed?
--
-- Also what mechanisms are there to alter e.g. the amplitude of 
-- a note. Potentially only type classes / method dictionaries 
-- can implement this if event is abstract.
--
data Event ot evt = Event
    { event_onset       :: !ot
    , event_body        :: !evt
    }
  deriving (Data,Eq,Show,Typeable)

