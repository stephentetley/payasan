{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Cadenza.Internal.Transform
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

module Payasan.LilyPond.Cadenza.Internal.Transform
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


import Payasan.LilyPond.Cadenza.Internal.Metrics
import Payasan.LilyPond.Cadenza.Internal.Syntax
import Payasan.LilyPond.Cadenza.Internal.Traversals

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Names.Interval
import Payasan.Base.Pitch
import Payasan.Base.ScaleDegree


-- | Double note lengths.
--
augment :: Phrase pch Duration anno -> Phrase pch Duration anno
augment = mapDuration doubleDuration

-- | Halve note lengths.
--
diminute :: Phrase pch Duration anno -> Phrase pch Duration anno
diminute = mapDuration halveDuration


-- | Transpose by an exact interval - this may produce 
-- non-scale tones.
--
transposeChromatic :: Interval 
                   -> Phrase Pitch drn anno 
                   -> Phrase Pitch drn anno
transposeChromatic ivl = mapPitch (.+^ ivl)




addDiatonicIntervalC :: ChromaticPitch -> DiatonicInterval -> ChromaticPitch
addDiatonicIntervalC (ChromaticPitch dp a) ivl = 
    ChromaticPitch (dp `addDiatonicInterval` ivl) a



transposeDiatonic :: DiatonicInterval 
                  -> Phrase Pitch drn anno 
                  -> Phrase Pitch drn anno
transposeDiatonic ivl ph = interScaleStep (mapPitch (`addDiatonicIntervalC` ivl)) ph



retrograde :: Phrase pch Duration anno -> Phrase pch Duration anno
retrograde (Phrase info gs) = Phrase info $ map revNG $ reverse gs
  where
    revNG (Atom e)          = Atom e
    revNG (Beamed es)       = Beamed $ map revNG $ reverse es
    revNG (Tuplet spec es)  = Tuplet spec $ map revNG $ reverse es


-- | Note - seems to need /scale degrees/ - taking interal with 
-- top note and adding same interval to lowest note does not work.
--
invertChromatic :: Phrase Pitch drn anno -> Phrase Pitch drn anno
invertChromatic ph = case lowestPitch ph of 
    Nothing -> ph
    Just p0 -> mapPitch (\ival -> p0 .+^ ival) $ intervalsFromTop ph


intervalsFromTop :: Phrase Pitch drn anno -> Phrase Interval drn anno
intervalsFromTop ph = case highestPitch ph of
    Nothing -> mapPitch (const perfect_unison) ph         -- notelist is empty or just rests
    Just top -> mapPitch (\p -> p `intervalBetween` top) ph 


-- | 08 Oct - this is now wrong due to changes to ScaleDegree!
--
invertDiatonic :: Phrase Pitch drn anno -> Phrase Pitch drn anno
invertDiatonic = interScaleStep invertDiatonic1

invertDiatonic1 :: Phrase ChromaticPitch drn anno -> Phrase ChromaticPitch drn anno
invertDiatonic1 ph = case lowestStep ph of 
    Nothing -> ph
    Just p0 -> mapPitch (\ival -> ChromaticPitch (p0 `addDiatonicInterval` ival) 0) $ diatonicsFromTop ph


diatonicsFromTop :: Phrase ChromaticPitch drn anno -> Phrase DiatonicInterval drn anno
diatonicsFromTop ph = case highestStep ph of
    Nothing -> mapPitch (const simple_unison) ph         -- notelist is empty or just rests
    Just top -> mapPitch (\p -> diatonic_base p `diatonicIntervalBetween` top) ph 



interScaleStep :: (Phrase ChromaticPitch drn anno -> Phrase ChromaticPitch drn anno)
               -> Phrase Pitch drn anno
               -> Phrase Pitch drn anno
interScaleStep fn = fromScaleStepRepr . fn . toScaleStepRepr

toScaleStepRepr :: Phrase Pitch drn anno -> Phrase ChromaticPitch drn anno
toScaleStepRepr = transformP step_algo
  where
    step_algo = MonoPitchAlgo { initial_stateP = ()
                              , element_trafoP = change
                              }

    change (Note p d a t)       = (\p1 -> Note p1 d a t) <$> mf p
    change (Rest d)             = pure $ Rest d
    change (Spacer d)           = pure $ Spacer d
    change (Skip d)             = pure $ Skip d
    change (Punctuation s)      = pure $ Punctuation s

    mf pch = (\key -> toChromaticPitch key pch) <$> asks info_key


fromScaleStepRepr :: Phrase ChromaticPitch drn anno -> Phrase Pitch drn anno
fromScaleStepRepr = transformP step_algo
  where
    step_algo = MonoPitchAlgo { initial_stateP = ()
                              , element_trafoP = change
                              }

    change (Note p d a t)       = (\p1 -> Note p1 d a t) <$> mf p
    change (Rest d)             = pure $ Rest d
    change (Spacer d)           = pure $ Spacer d
    change (Skip d)             = pure $ Skip d
    change (Punctuation s)      = pure $ Punctuation s

    mf oss = (\key -> fromChromaticPitch key oss) <$> asks info_key

