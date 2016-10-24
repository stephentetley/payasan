{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Cadenza.Internal.Transform
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
import Payasan.Base.Internal.Scale

import Payasan.Base.Duration
import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Names.Interval
import Payasan.Base.Pitch
import Payasan.Base.Diatonic


-- | Double note lengths.
--
augment :: Part pch Duration anno -> Part pch Duration anno
augment = mapDuration doubleDuration

-- | Halve note lengths.
--
diminute :: Part pch Duration anno -> Part pch Duration anno
diminute = mapDuration halveDuration



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

retrograde :: Part pch Duration anno -> Part pch Duration anno
retrograde (Part info gs) = Part info $ map revNG $ reverse gs
  where
    revNG (Atom e)          = Atom e
    revNG (Beamed cs)       = Beamed $ map revNG $ reverse cs
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
    step_algo = CadenzaPitchAlgo { initial_stateP = ()
                              , element_trafoP = change }

    change (Note p d a t)       = (\p1 -> Note p1 d a t) <$> mf p
    change (Rest d)             = pure $ Rest d
    change (Spacer d)           = pure $ Spacer d
    change (Skip d)             = pure $ Skip d
    change (Punctuation s)      = pure $ Punctuation s

    -- This is poor, scale should be built just once...
    mf pch = (\key -> toDiatonic (buildScale key) pch) <$> asks section_key


fromDiatonicPart :: Part Diatonic drn anno -> Part Pitch drn anno
fromDiatonicPart = transformP step_algo
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

