{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.Transform
-- Copyright   :  (c) Stephen Tetley 2015-2017
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


import Payasan.Base.Duration
import Payasan.Base.Diatonic
import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Names.Interval
import Payasan.Base.Pitch
import Payasan.Base.Scale


-- | Double note lengths (and recalc bars).
--
augment :: Section pch Duration anno -> Section pch Duration anno
augment = recalcBars . mapDuration doubleDuration

-- | Halve note lengths (and recalc bars).
--
diminute :: Section pch Duration anno -> Section pch Duration anno
diminute = recalcBars . mapDuration halveDuration


-- | Transpose by an exact interval - this may produce 
-- non-scale tones.
--
transposeChromatic :: Interval 
                   -> Section Pitch drn anno 
                   -> Section Pitch drn anno
transposeChromatic ivl = transformPitch (.+^ ivl)


transposeDiatonic :: DiatonicInterval 
                  -> Section Pitch drn anno 
                  -> Section Pitch drn anno
transposeDiatonic ivl ph = diatonically (transformPitch (`addDiatonicInterval` ivl)) ph


-- | Reverse
--
retrograde :: Section pch Duration anno -> Section pch Duration anno
retrograde (Section { section_name = name 
                    , section_info = info 
                    , section_bars = bs }) = 
    Section { section_name = name
            , section_info = info 
            , section_bars = map revBar $ reverse bs
            }
  where
    revBar (Bar es)         = Bar $ map revNG $ reverse es
    
    revNG (Atom e)          = Atom e
    revNG (Tuplet spec es)  = Tuplet spec $ reverse es



-- | Note - seems to need /scale degrees/ - taking interval with 
-- top note and adding same interval to lowest note does not work.
--
invertChromatic :: Section Pitch drn anno -> Section Pitch drn anno
invertChromatic ph = case lowestPitch ph of 
    Nothing -> ph
    Just p0 -> transformPitch (\ival -> p0 .+^ ival) $ intervalsFromTop ph


intervalsFromTop :: Section Pitch drn anno -> Section Interval drn anno
intervalsFromTop ph = case highestPitch ph of
    Nothing -> transformPitch (const perfect_unison) ph         -- notelist is empty or just rests
    Just top -> transformPitch (\p -> p `intervalBetween` top) ph 


invertDiatonic :: Section Pitch drn anno -> Section Pitch drn anno
invertDiatonic = diatonically invertDiatonic1



invertDiatonic1 :: Section Diatonic drn anno -> Section Diatonic drn anno
invertDiatonic1 ph = case lowestDiatonic ph of 
    Nothing -> ph
    Just p0 -> transformPitch (\ival -> p0 `addDiatonicInterval` ival) $ diatonicsFromTop ph



diatonicsFromTop :: Section Diatonic drn anno -> Section DiatonicInterval drn anno
diatonicsFromTop ph = case highestDiatonic ph of
    Nothing -> transformPitch (const simple_unison) ph         -- notelist is empty or just rests
    Just top -> transformPitch (\p -> p `diatonicIntervalBetween` top) ph 



diatonically :: (Section Diatonic drn anno -> Section Diatonic drn anno)
             -> Section Pitch drn anno
             -> Section Pitch drn anno
diatonically fn = fromDiatonicSection . fn . toDiatonicSection


toDiatonicSection :: Section Pitch drn anno -> Section Diatonic drn anno
toDiatonicSection = transformPitchCtx cxf toDiatonic
  where
    cxf = buildScale . sectionKeyWithDefault c_maj

fromDiatonicSection :: Section Diatonic drn anno -> Section Pitch drn anno
fromDiatonicSection = transformPitchCtx cxf fromDiatonic
  where
    cxf = buildScale . sectionKeyWithDefault c_maj
