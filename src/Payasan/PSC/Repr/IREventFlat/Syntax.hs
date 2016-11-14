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
-- Flat eventlist syntax (no metrical division into bars).
-- Intended as the final intermediate representation before 
-- rendering to an audio format - e.g. Csound or MIDI.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IREventFlat.Syntax
  ( 
    Part(..)
  , Event(..)

  , sortByOnset
  , groupByEventBody
  ) where


import Data.Data


-- Parametric on time type to model seconds or MIDI ticks.
--
-- Interpretion of the onset time is left to the processing 
-- function - it could represent absolute times or delta times.
--
-- Duration is hidden (think sustenueto), but it will be 
-- inspected by the rendering dictionary, which will know the
-- concrete instantiation of the event_body.

data Part ot evt = Part { part_events :: [Event ot evt] }
  deriving (Data,Eq,Show,Typeable)


data Event ot evt = Event
    { event_onset      :: ot
    , event_body       :: evt
    }
  deriving (Data,Eq,Show,Typeable)


  
sortByOnset :: Ord ot => Part ot evt -> Part ot evt
sortByOnset (Part es) = Part $ sortBy fn es
  where
    fn (Event ot1 _) (Event ot2 _) = compare ot1 ot2

-- | Note this is intentionally oblivious to onset time.    
groupByEventBody :: (evt -> evt -> Bool) Part ot evt -> [Part ot evt]
groupByEventBoby evF (Part es) = map Part $ groupBy (adapt evF)
  where
    adapt (Event _ e1) (Event _ e2) = evF e1 e2
    
    
    