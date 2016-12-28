{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IRSimpleTile.Onsets
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Calculate onsets for tiled repr.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IRSimpleTile.Onsets
  ( 
    Part(..)
  , Section(..)
  , Bar(..)
  
  , onsets

  ) where

import qualified Payasan.PSC.Repr.IRSimpleTile.Syntax as T

import Payasan.Base.Basis

import Data.Data
import Data.List (mapAccumL)


data Part = Part { part_sections :: [Section] }
  deriving (Data,Eq,Show,Typeable)

-- | We keep section in this syntax. Having named sections is
-- expected to allow transformations limited to a specific region.
--
data Section = Section 
    { section_name      :: !String
    , section_onset     :: !Seconds
    , section_bars      :: [Bar]
    }
  deriving (Data,Eq,Show,Typeable)
  
  
data Bar = Bar
    { bar_onset                 :: !Seconds
    , bar_element_onsets        :: [Seconds]
    }
  deriving (Data,Eq,Show,Typeable)



--------------------------------------------------------------------------------
-- Calculate onsets

onsets :: T.Part pch anno -> Part
onsets (T.Part {T.part_sections = ss}) = 
    Part { part_sections = snd $ mapAccumL sectionA 0 ss }


sectionA :: Seconds -> T.Section pch anno -> (Seconds,Section)
sectionA ot (T.Section { T.section_name = name
                       , T.section_bars = bs }) = (ot1,section1)
  where
    (ot1,bars)  = mapAccumL barA ot bs
    section1    = Section { section_name  = name
                          , section_onset = ot
                          , section_bars  = bars
                          }


-- | The bar has an absolute onset, but the onsets of the
-- elements are deltas from the styart of the bar.
--
barA :: Seconds -> T.Bar pch anno -> (Seconds,Bar)
barA ot (T.Bar {T.bar_elems = es}) = (ot + bar_drn, bar1)
  where
    bar_drn     = sum $ map T.elementDuration es
    (_, elems)  = mapAccumL elementA 0 es
    bar1        = Bar { bar_onset = ot
                      , bar_element_onsets = elems 
                      }

elementA :: Seconds -> T.Element pch anno -> (Seconds,Seconds)
elementA ot e = (ot, ot + T.elementDuration e)
