{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.Transform
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

module Payasan.Score.Cadenza.Internal.Transform
  (
    augment
  , diminute

  , transposeChromatic
  , transposeDiatonic

  , retrograde
  , invertChromatic
  , invertDiatonic

  , diatonicsFromTop -- temp

  ) where


import Payasan.Score.Cadenza.Internal.Metrics
import Payasan.Score.Cadenza.Internal.Syntax
import Payasan.Score.Cadenza.Internal.Traversals

import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Diatonic
import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Names.Interval
import Payasan.Base.Pitch
import Payasan.Base.Scale

import Control.Monad.Reader             -- package: mtl

-- | Double note lengths.
--
augment :: Section pch Duration anno -> Section pch Duration anno
augment = mapDuration doubleDuration

-- | Halve note lengths.
--
diminute :: Section pch Duration anno -> Section pch Duration anno
diminute = mapDuration halveDuration



-- | Transpose by an exact interval - this may produce 
-- non-scale tones.
--
transposeChromatic :: Interval 
                   -> Section Pitch drn anno 
                   -> Section Pitch drn anno
transposeChromatic ivl = mapPitch (.+^ ivl)


transposeDiatonic :: DiatonicInterval 
                  -> Section Pitch drn anno 
                  -> Section Pitch drn anno
transposeDiatonic ivl ph = diatonically (mapPitch (`addDiatonicInterval` ivl)) ph

retrograde :: Section pch Duration anno -> Section pch Duration anno
retrograde (Section info gs) = Section info $ map revNG $ reverse gs
  where
    revNG (Atom e)          = Atom e
    revNG (Beamed cs)       = Beamed $ map revNG $ reverse cs
    revNG (Tuplet spec es)  = Tuplet spec $ reverse es



-- | Note - seems to need /scale degrees/ - taking interval with 
-- top note and adding same interval to lowest note does not work.
--
invertChromatic :: Section Pitch drn anno -> Section Pitch drn anno
invertChromatic ph = case lowestPitch ph of 
    Nothing -> ph
    Just p0 -> mapPitch (\ival -> p0 .+^ ival) $ intervalsFromTop ph


intervalsFromTop :: Section Pitch drn anno -> Section Interval drn anno
intervalsFromTop ph = case highestPitch ph of
    Nothing -> mapPitch (const perfect_unison) ph         -- notelist is empty or just rests
    Just top -> mapPitch (\p -> p `intervalBetween` top) ph 


invertDiatonic :: Section Pitch drn anno -> Section Pitch drn anno
invertDiatonic = diatonically invertDiatonic1



invertDiatonic1 :: Section Diatonic drn anno -> Section Diatonic drn anno
invertDiatonic1 ph = case lowestDiatonic ph of 
    Nothing -> ph
    Just p0 -> mapPitch (\ival -> p0 `addDiatonicInterval` ival) $ diatonicsFromTop ph



diatonicsFromTop :: Section Diatonic drn anno -> Section DiatonicInterval drn anno
diatonicsFromTop ph = case highestDiatonic ph of
    Nothing -> mapPitch (const simple_unison) ph         -- notelist is empty or just rests
    Just top -> mapPitch (\p -> p `diatonicIntervalBetween` top) ph 



diatonically :: (Section Diatonic drn anno -> Section Diatonic drn anno)
             -> Section Pitch drn anno
             -> Section Pitch drn anno
diatonically fn = fromDiatonicSection . fn . toDiatonicSection


toDiatonicSection :: Section Pitch drn anno -> Section Diatonic drn anno
toDiatonicSection = transformP step_algo
  where
    step_algo = CadenzaPitchAlgo { initial_stateP = ()
                              , element_trafoP = change }

    change (Note p d a t)       = (\p1 -> Note p1 d a t) <$> mf p
    change (Rest d)             = pure $ Rest d
    change (Spacer d)           = pure $ Spacer d
    change (Skip d)             = pure $ Skip d
    change (Punctuation s)      = pure $ Punctuation s

    -- This is poor, scale should be built just once...
    mf pch = (\key -> toDiatonic (buildScale key) pch) <$> asks section_key


fromDiatonicSection :: Section Diatonic drn anno -> Section Pitch drn anno
fromDiatonicSection = transformP step_algo
  where
    step_algo = CadenzaPitchAlgo { initial_stateP = ()
                              , element_trafoP = change }

    change (Note p d a t)       = (\p1 -> Note p1 d a t) <$> mf p
    change (Rest d)             = pure $ Rest d
    change (Spacer d)           = pure $ Spacer d
    change (Skip d)             = pure $ Skip d
    change (Punctuation s)      = pure $ Punctuation s

    -- This is poor, scale should be built just once...
    mf oss = (\key -> fromDiatonic (buildScale key) oss) <$> asks section_key

