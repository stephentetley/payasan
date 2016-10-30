{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.Transform
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Musical transfomations (augmentation, diminution etc.)
--
--------------------------------------------------------------------------------

module Payasan.Score.Elementary.Internal.Transform
  (
    augment
  , diminute

  , transposeChromatic
  , transposeDiatonic

  , retrograde
  , invertChromatic
  , invertDiatonic


  ) where


import Payasan.Score.Elementary.Internal.Metrics
import Payasan.Score.Elementary.Internal.RecalcBars
import Payasan.Score.Elementary.Internal.Syntax
import Payasan.Score.Elementary.Internal.Traversals

import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Internal.Scale

import Payasan.Base.Duration
import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Names.Interval
import Payasan.Base.Pitch
import Payasan.Base.Diatonic


-- | Double note lengths (and recalc bars).
--
augment :: Part pch Duration anno -> Part pch Duration anno
augment = recalcBars . mapDuration doubleDuration

-- | Halve note lengths (and recalc bars).
--
diminute :: Part pch Duration anno -> Part pch Duration anno
diminute = recalcBars . mapDuration halveDuration


-- | Transpose by an exact interval - this may produce 
-- non-scale tones.
--
transposeChromatic :: Interval 
                   -> Part Pitch drn anno 
                   -> Part Pitch drn anno
transposeChromatic ivl = transformPitch (.+^ ivl)


transposeDiatonic :: DiatonicInterval 
                  -> Part Pitch drn anno 
                  -> Part Pitch drn anno
transposeDiatonic ivl ph = diatonically (transformPitch (`addDiatonicInterval` ivl)) ph


-- | Reverse
--
retrograde :: Part pch Duration anno -> Part pch Duration anno
retrograde (Part info bs) = Part info $ map revBar $ reverse bs
  where
    revBar (Bar es)         = Bar $ map revNG $ reverse es
    
    revNG (Atom e)          = Atom e
    revNG (Tuplet spec es)  = Tuplet spec $ reverse es



-- | Note - seems to need /scale degrees/ - taking interval with 
-- top note and adding same interval to lowest note does not work.
--
invertChromatic :: Part Pitch drn anno -> Part Pitch drn anno
invertChromatic ph = case lowestPitch ph of 
    Nothing -> ph
    Just p0 -> transformPitch (\ival -> p0 .+^ ival) $ intervalsFromTop ph


intervalsFromTop :: Part Pitch drn anno -> Part Interval drn anno
intervalsFromTop ph = case highestPitch ph of
    Nothing -> transformPitch (const perfect_unison) ph         -- notelist is empty or just rests
    Just top -> transformPitch (\p -> p `intervalBetween` top) ph 


invertDiatonic :: Part Pitch drn anno -> Part Pitch drn anno
invertDiatonic = diatonically invertDiatonic1



invertDiatonic1 :: Part Diatonic drn anno -> Part Diatonic drn anno
invertDiatonic1 ph = case lowestDiatonic ph of 
    Nothing -> ph
    Just p0 -> transformPitch (\ival -> p0 `addDiatonicInterval` ival) $ diatonicsFromTop ph



diatonicsFromTop :: Part Diatonic drn anno -> Part DiatonicInterval drn anno
diatonicsFromTop ph = case highestDiatonic ph of
    Nothing -> transformPitch (const simple_unison) ph         -- notelist is empty or just rests
    Just top -> transformPitch (\p -> p `diatonicIntervalBetween` top) ph 



diatonically :: (Part Diatonic drn anno -> Part Diatonic drn anno)
             -> Part Pitch drn anno
             -> Part Pitch drn anno
diatonically fn = fromDiatonicPart . fn . toDiatonicPart


toDiatonicPart :: Part Pitch drn anno -> Part Diatonic drn anno
toDiatonicPart = transformPitchCtx cxf toDiatonic
  where
    cxf = buildScale . section_key

fromDiatonicPart :: Part Diatonic drn anno -> Part Pitch drn anno
fromDiatonicPart = transformPitchCtx cxf fromDiatonic
  where
    cxf = buildScale . section_key
