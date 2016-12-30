{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventBar.Syntax
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Eventlist syntax that retains some structure (sections, bars). 
-- Intended for low level metrical and articulation transforms.
-- E.g. de-quantization / rubato.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IREventBar.Syntax
  ( 
    Part(..)
  , Section(..)
  , Bar(..)
  , Event(..)
  , EventBody(..)

  ) where

import Payasan.Base.Basis

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


-- REVISED - It is likely we can fix duration/onset in this 
-- representation to seconds...
--
data Part pch anno = Part { part_sections :: [Section pch anno] }
  deriving (Data,Eq,Show,Typeable)


-- | We keep section in this syntax. 
-- Having named sections could be helpful to make the generated 
-- Csound readable.
--
data Section pch anno = Section
    { section_name      :: !String
    , section_onset     :: !Seconds
    , section_bars      :: [Bar pch anno]
    }
  deriving (Data,Eq,Show,Typeable)

  
data Bar pch anno = Bar
    { bar_onset         :: !Seconds
    , bar_events        :: [Event pch anno]
    }
  deriving (Data,Eq,Show,Typeable)




-- | Distinguish between normal events and graces
-- (the later cannot be accented).

data Event pch anno = Event 
    { event_onset       :: !Seconds
    , event_body        :: EventBody pch anno
    }
  deriving (Data,Eq,Show,Typeable)


data EventBody pch anno = Event1 pch !Seconds anno
                        | EventGrace pch !Seconds
  deriving (Data,Eq,Show,Typeable)

