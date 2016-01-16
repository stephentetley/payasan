{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.Transform
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

module Payasan.Base.Elementary.Internal.Transform
  (
    augment
  , diminute

  , transposeChromatic
  , transposeDiatonic

  , retrograde
  , invertChromatic
  , invertDiatonic


  ) where


import Payasan.Base.Elementary.Internal.Metrics
import Payasan.Base.Elementary.Internal.RecalcBars
import Payasan.Base.Elementary.Internal.Syntax
import Payasan.Base.Elementary.Internal.Traversals

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

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
transposeChromatic ivl = mapPitch (.+^ ivl)


transposeDiatonic :: DiatonicInterval 
                  -> Part Pitch drn anno 
                  -> Part Pitch drn anno
transposeDiatonic ivl ph = diatonically (mapPitch (`addDiatonicInterval` ivl)) ph


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
    Just p0 -> mapPitch (\ival -> p0 .+^ ival) $ intervalsFromTop ph


intervalsFromTop :: Part Pitch drn anno -> Part Interval drn anno
intervalsFromTop ph = case highestPitch ph of
    Nothing -> mapPitch (const perfect_unison) ph         -- notelist is empty or just rests
    Just top -> mapPitch (\p -> p `intervalBetween` top) ph 


invertDiatonic :: Part Pitch drn anno -> Part Pitch drn anno
invertDiatonic = diatonically invertDiatonic1



invertDiatonic1 :: Part Diatonic drn anno -> Part Diatonic drn anno
invertDiatonic1 ph = case lowestDiatonic ph of 
    Nothing -> ph
    Just p0 -> mapPitch (\ival -> p0 `addDiatonicInterval` ival) $ diatonicsFromTop ph



diatonicsFromTop :: Part Diatonic drn anno -> Part DiatonicInterval drn anno
diatonicsFromTop ph = case highestDiatonic ph of
    Nothing -> mapPitch (const simple_unison) ph         -- notelist is empty or just rests
    Just top -> mapPitch (\p -> p `diatonicIntervalBetween` top) ph 



diatonically :: (Part Diatonic drn anno -> Part Diatonic drn anno)
             -> Part Pitch drn anno
             -> Part Pitch drn anno
diatonically fn = fromDiatonicPart . fn . toDiatonicPart


toDiatonicPart :: Part Pitch drn anno -> Part Diatonic drn anno
toDiatonicPart = transformP step_algo
  where
    step_algo = ElemPitchAlgo { initial_stateP = ()
                              , element_trafoP = change }

    change (Note p d a t)       = (\p1 -> Note p1 d a t) <$> mf p
    change (Rest d)             = pure $ Rest d
    change (Spacer d)           = pure $ Spacer d
    change (Skip d)             = pure $ Skip d
    change (Punctuation s)      = pure $ Punctuation s

    mf pch = (\key -> toDiatonic key pch) <$> asks section_key


fromDiatonicPart :: Part Diatonic drn anno -> Part Pitch drn anno
fromDiatonicPart = transformP step_algo
  where
    step_algo = ElemPitchAlgo { initial_stateP = ()
                              , element_trafoP = change }

    change (Note p d a t)       = (\p1 -> Note p1 d a t) <$> mf p
    change (Rest d)             = pure $ Rest d
    change (Spacer d)           = pure $ Spacer d
    change (Skip d)             = pure $ Skip d
    change (Punctuation s)      = pure $ Punctuation s

    mf oss = (\key -> fromDiatonic key oss) <$> asks section_key

