{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.Transform
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Musical transfomations (augmentation, diminution etc.)
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.Transform
  (
    augment
  , diminute

  , transposeChromatic

  , retrograde
  , invertMelody

  ) where


import Payasan.Base.Monophonic.Internal.Metrics
import Payasan.Base.Monophonic.Internal.RecalcBars
import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Monophonic.Internal.Traversals

import Payasan.Base.Duration
import Payasan.Base.Names.Interval
import Payasan.Base.Pitch


-- | Double note lengths (and recalc bars).
--
augment :: Phrase pch Duration anno -> Phrase pch Duration anno
augment = recalcBars . mapDuration doubleDuration

-- | Halve note lengths (and recalc bars).
--
diminute :: Phrase pch Duration anno -> Phrase pch Duration anno
diminute = recalcBars . mapDuration halveDuration



-- | Transpose by an exact interval - this may produce 
-- non-scale tones.
--
transposeChromatic :: Interval 
                   -> Phrase Pitch drn anno 
                   -> Phrase Pitch drn anno
transposeChromatic ivl = mapPitch (.+^ ivl)


-- TODO - need more apparatus for transposeDiatonic

retrograde :: Phrase pch Duration anno -> Phrase pch Duration anno
retrograde (Phrase bs) = Phrase $ map revBar $ reverse bs
  where
    revBar (Bar info es)    = Bar info $ map revNG $ reverse es
    
    revNG (Atom e)          = Atom e
    revNG (Tuplet spec es)  = Tuplet spec $ map revNG $ reverse es


invertMelody :: Phrase Pitch drn anno -> Phrase Pitch drn anno
invertMelody ph = case lowestPitch ph of 
    Nothing -> ph
    Just p0 -> mapPitch (\ival -> p0 .+^ ival) $ intervalsFromTop ph


intervalsFromTop :: Phrase Pitch drn anno -> Phrase Interval drn anno
intervalsFromTop ph = case highestPitch ph of
    Nothing -> mapPitch (const perfect_unison) ph         -- notelist is empty or just rests
    Just top -> mapPitch (\p -> p `intervalBetween` top) ph 