{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Melody.Transform
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Transform melodies.
--
--------------------------------------------------------------------------------

module Payasan.Models.Melody.Transform
  (
        
    augment
  , diminute
  , transposeChromatic

  ) where


import Payasan.Models.Melody.Base

import Payasan.Base

augment :: Melody -> Melody
augment = genAugment 2.0

genAugment :: Double -> Melody -> Melody
genAugment dx = mapDuration (\d -> d * realToFrac dx)

diminute :: Melody -> Melody
diminute = genDiminute 2.0

genDiminute :: Double -> Melody -> Melody
genDiminute dx = mapDuration (\d -> d / realToFrac dx)


transposeChromatic :: Int -> Melody -> Melody
transposeChromatic s = mapNote (addSemitones s) 
