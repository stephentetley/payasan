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
  , EventBody(..)
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
-- Duration is hidden (think sustenueto), but it will be 
-- inspected by the rendering dictionary, which will know the
-- concrete instantiation of the event_body.

data Part pch time anno = Part 
    { part_sections     :: [Section pch time anno] 
    }
  deriving (Data,Eq,Show,Typeable)


data Section pch time anno = Section
    { section_name      :: !String
    , section_events    :: [Event pch time anno]
    }
  deriving (Data,Eq,Show,Typeable)


data Event pch time anno = Event
    { event_onset      :: time
    , event_body       :: EventBody pch time anno
    }
  deriving (Data,Eq,Show,Typeable)


data EventBody pch time anno = Event1 pch time anno
                             | EventGrace pch time
  deriving (Data,Eq,Show,Typeable)

  
sortByOnset :: Ord time => Part pch time anno -> Part pch time anno
sortByOnset p@(Part { part_sections = ss }) = 
    p { part_sections = map sortSection ss }
  where
    sortSection s@(Section {section_events = es}) = 
        s { section_events = sortBy cmp es }

    cmp (Event ot1 _) (Event ot2 _) = compare ot1 ot2
