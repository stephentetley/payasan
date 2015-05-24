{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Metronome
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Metronome
--
--------------------------------------------------------------------------------

module Payasan.Models.Metronome
  ( 

    TickF 
  , Metronome
  , MetroPhrase
  , Tick(..)
  , metroPhrase
  , renderMetroPhrase
  , metro

  ) where


import Payasan.Models.Metronome.Base
import Payasan.Models.Metronome.Interpret

import Payasan.Base
import Payasan.Base.Internal.Utils


metro :: TimeSig -> Metronome 
metro (n,d) = Metronome $ wrapCat $ [bar1]
  where
    bar1 = Bar { bar_tick_is  = intToBeat d
               , bar_ticks    = makeTicks n
               }


makeTicks :: Int -> [Tick]
makeTicks i | i < 1     = []
            | otherwise = STRONG_TICK : replicate (i-1) TICK
