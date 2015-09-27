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
  , transposeDiatonic

  , retrograde
  , invertChromatic


  ) where


import Payasan.Base.Monophonic.Internal.Metrics
import Payasan.Base.Monophonic.Internal.RecalcBars
import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Monophonic.Internal.Traversals

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import Payasan.Base.Names.Interval
import Payasan.Base.Pitch
import Payasan.Base.ScaleDegree


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


transposeDiatonic :: DiatonicInterval 
                  -> Phrase Pitch drn anno 
                  -> Phrase Pitch drn anno
transposeDiatonic ivl ph = interScaleStep (mapPitch (`addDiatonicInterval` ivl)) ph


retrograde :: Phrase pch Duration anno -> Phrase pch Duration anno
retrograde (Phrase bs) = Phrase $ map revBar $ reverse bs
  where
    revBar (Bar info es)    = Bar info $ map revNG $ reverse es
    
    revNG (Atom e)          = Atom e
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




interScaleStep :: (Phrase OveScaleStep drn anno -> Phrase OveScaleStep drn anno)
               -> Phrase Pitch drn anno
               -> Phrase Pitch drn anno
interScaleStep fn = fromScaleStepRepr . fn . toScaleStepRepr

toScaleStepRepr :: Phrase Pitch drn anno -> Phrase OveScaleStep drn anno
toScaleStepRepr = transformP step_algo
  where
    step_algo = MonoPitchAlgo { initial_stateP = ()
                              , element_trafoP = change
                              }

    change (Note p d a) = (\p1 -> Note p1 d a) <$> mf p
    change (Rest d)     = pure $ Rest d

    mf pch = (\key -> fromPitch key pch) <$> asksLocal local_key


fromScaleStepRepr :: Phrase OveScaleStep drn anno -> Phrase Pitch drn anno
fromScaleStepRepr = transformP step_algo
  where
    step_algo = MonoPitchAlgo { initial_stateP = ()
                              , element_trafoP = change
                              }

    change (Note p d a) = (\p1 -> Note p1 d a) <$> mf p
    change (Rest d)     = pure $ Rest d

    mf oss = (\key -> toPitch key oss) <$> asksLocal local_key
