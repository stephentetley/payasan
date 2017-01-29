{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventFlat.Syntax
-- Copyright   :  (c) Stephen Tetley 2016-2017
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
-- Potentially it should support prettifying the output 
-- e.g. ellipses in subsquent Csound rows.
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

data Part onset drn body = Part 
    { part_sections     :: [Section onset drn body] 
    }
  deriving (Data,Eq,Show,Typeable)


data Section onset drn body = Section
    { section_name      :: !String
    , section_events    :: [Event onset drn body]
    }
  deriving (Data,Eq,Show,Typeable)

-- Events are parametric on body - essentially this means we 
-- don't know anything about them.
--
-- (This would need a rewrite function during conversion from 
-- IREventBar)
--
-- Body might be a list, a map or a data type...
--
data Event onset drn body = Event
    { event_onset       :: onset
    , event_duration    :: drn
    , event_body        :: body
    }
  deriving (Data,Eq,Ord,Show,Typeable)



  
sortByOnset :: Ord onset => Part onset drn body -> Part onset drn body
sortByOnset p@(Part { part_sections = ss }) = 
    p { part_sections = map sortSection ss }
  where
    sortSection s@(Section {section_events = es}) = 
        s { section_events = sortBy cmp es }

    cmp e1 e2 = compare (event_onset e1) (event_onset e2)

