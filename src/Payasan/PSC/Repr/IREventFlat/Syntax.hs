{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventFlat.Syntax
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Flat eventlist syntax (no metrical division into bars, but 
-- sections remain - helpful to track in output).
--
-- Time (duration and onset) is parametric again.
--
-- Intended as the final intermediate representation before 
-- rendering to an audio format - e.g. Csound or MIDI.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IREventFlat.Syntax
  ( 
    Part(..)
  , Section(..)
  , Event(..)
  , sortByOnset

  ) where

import Data.List
import Data.Data


-- Parametric on event onset time type to model seconds or 
-- MIDI ticks.
--
-- Interpretion of the onset time is left to the processing 
-- function - it could represent absolute times or delta times.
--
-- Onset, pitch and duration are expected to be changed to other 
-- units (e.g. Seconds to MidiTicks) but not otherwise 
-- manipulated.
--

data Part onset pch drn anno = Part 
    { part_sections     :: [Section onset pch drn anno] 
    }
  deriving (Data,Eq,Show,Typeable)


data Section onset pch drn anno = Section
    { section_name      :: !String
    , section_events    :: [Event onset pch drn anno]
    }
  deriving (Data,Eq,Show,Typeable)


data Event onset pch drn anno =
      Event onset pch drn anno
    | Grace onset pch drn
  deriving (Data,Eq,Show,Typeable)

  
sortByOnset :: Ord onset => Part onset pch drn anno -> Part onset pch drn anno
sortByOnset p@(Part { part_sections = ss }) = 
    p { part_sections = map sortSection ss }
  where
    sortSection s@(Section {section_events = es}) = 
        s { section_events = sortBy cmp es }

    cmp e1 e2 = compare (getOnset e1) (getOnset e2)


getOnset :: Event onset pch drn anno -> onset
getOnset (Event ot _ _ _)       = ot
getOnset (Grace ot _ _)         = ot
