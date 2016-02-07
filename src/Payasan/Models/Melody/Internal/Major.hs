{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Melody.Internal.Major
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Major - stability for identifying cadences, etc.
--
--------------------------------------------------------------------------------

module Payasan.Models.Melody.Internal.Major
  ( 

    Major(..)

  , isStable
  , isUnstable

  , stability
  , resolve

  ) where


import Payasan.Base.Diatonic
import Payasan.Base.Names.DiatonicInterval


import Data.Data


newtype Major = Major { getMajor :: Diatonic }
  deriving (Data,Eq,Show,Typeable)


-- | All chromatic tones are unstable...
--
isStable :: Major -> Bool
isStable = step . getMajor
  where
    step p | isDiatonic p = let i = scaleDegreeInt p 
                            in i == 1 || i == 3 || i == 5
           | otherwise    = False

isUnstable :: Major -> Bool
isUnstable = not . isStable



-- | 1 is tonic - 7 is leading tone, 8 is any chromatic...
--
stability :: Major -> Int
stability (Major p) = if isDiatonic p then step (scaleDegreeInt p) else 8
  where
    step 1  = 1
    step 2  = 5
    step 3  = 3
    step 4  = 6
    step 5  = 2
    step 6  = 4
    step 7  = 7
    step _  = error $ "stability - unreachable" ++ show p




-- | Chromatic - flats go down, sharps go up
--
resolve :: Major -> Major
resolve (Major p) = Major $ if isDiatonic p then stepD (scaleDegreeInt p) else stepC
  where
    stepD 1 = p
    stepD 2 = resolveDown p
    stepD 3 = p
    stepD 4 = resolveDown p
    stepD 5 = p
    stepD 6 = resolveDown p
    stepD 7 = resolveUp p
    stepD _ = error $ "resolve - unreachable" ++ show p

    stepC | isChromaticFlat p = resolveDown $ nubAlteration p
          | otherwise         = resolveUp   $ nubAlteration p


resolveDown :: Diatonic -> Diatonic
resolveDown = (`subDiatonicInterval` simple_second)

resolveUp :: Diatonic -> Diatonic
resolveUp = (`addDiatonicInterval` simple_second)

