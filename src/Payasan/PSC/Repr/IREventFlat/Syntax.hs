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
-- Flat Event list syntax (no metrical division into bars).
-- For audition, e.g. Csound or MIDI
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IREventFlat.Syntax
  ( 
    Part(..)
  , Event(..)

  ) where


import Data.Data


-- Design note
-- This is the very low level event list to support e.g. MIDI or Csound
-- generation.



-- Parametric on time to model (pre-) MIDI upto delta repr.
-- Allow onset time and duration to be different time representations

data Part ot drn note = Part { part_events :: [Event ot drn note] }
  deriving (Data,Eq,Show,Typeable)


data Event ot drn note = Event
    { event_onset      :: ot
    , event_duration   :: drn
    , event_note       :: note
    }
  deriving (Data,Eq,Show,Typeable)

