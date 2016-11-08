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
-- Eventlist syntax - for low level metrical and articulation transforms.
-- E.g. quantization.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IREventBeam.Syntax
  ( 
    Part(..)
  , Bar(..)
  , Event(..)

  ) where

import Data.Data

-- Design note
-- The point of an eventlist representation is to support temporal
-- and expressive rendering (e.g quantization, accentation) and potentially
-- crescendos etc. Hence Bar is included.



-- Parametric on onset time which may be a different type to duration

data Part ot drn note = Part { part_bars :: [Bar ot drn note] }
  deriving (Data,Eq,Show,Typeable)

data Bar ot drn note = Bar
    { bar_onset         :: ot
    , bar_events        :: [Event ot drn note]
    }
  deriving (Data,Eq,Show,Typeable)


data Event ot drn note = Event
    { event_delta_time  :: ot
    , event_duration    :: drn
    , event_note        :: note
    }
  deriving (Data,Eq,Show,Typeable)

