{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Names.DiatonicInterval
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Diatonic interval names
--
--------------------------------------------------------------------------------

module Payasan.Base.Names.DiatonicInterval
  ( 

  -- * Simple intervals
    simple_unison
  , simple_second
  , simple_third
  , simple_fourth
  , simple_fifth
  , simple_sixth
  , simple_seventh

  -- * Compound intervals (constructors)
  , compoundUnison
  , compoundSecond
  , compoundThird
  , compoundFourth
  , compoundFifth
  , compoundSixth
  , compoundSeventh


  ) where


import Payasan.Base.Diatonic



simple_unison       :: DiatonicInterval
simple_unison       = DiatonicInterval { dia_interval_type   = UNISON
                                       , dia_interval_octave = 0 }

simple_second       :: DiatonicInterval
simple_second       = DiatonicInterval { dia_interval_type   = SECOND
                                       , dia_interval_octave = 0 }

simple_third        :: DiatonicInterval
simple_third        = DiatonicInterval { dia_interval_type   = THIRD
                                       , dia_interval_octave = 0 }

simple_fourth       :: DiatonicInterval
simple_fourth       = DiatonicInterval { dia_interval_type   = FOURTH
                                       , dia_interval_octave = 0 }

simple_fifth        :: DiatonicInterval
simple_fifth        = DiatonicInterval { dia_interval_type   = FIFTH
                                       , dia_interval_octave = 0 }

simple_sixth        :: DiatonicInterval
simple_sixth        = DiatonicInterval { dia_interval_type   = SIXTH
                                       , dia_interval_octave = 0 }

simple_seventh      :: DiatonicInterval
simple_seventh      = DiatonicInterval { dia_interval_type   = SEVENTH
                                       , dia_interval_octave = 0 }


compoundUnison      :: Int -> DiatonicInterval
compoundUnison o    = DiatonicInterval { dia_interval_type   = UNISON
                                       , dia_interval_octave = o }

compoundSecond      :: Int -> DiatonicInterval
compoundSecond o    = DiatonicInterval { dia_interval_type   = SECOND
                                       , dia_interval_octave = o }

compoundThird       :: Int -> DiatonicInterval
compoundThird o     = DiatonicInterval { dia_interval_type   = THIRD
                                       , dia_interval_octave = o }

compoundFourth      :: Int -> DiatonicInterval
compoundFourth o    = DiatonicInterval { dia_interval_type   = FOURTH
                                       , dia_interval_octave = o }

compoundFifth       :: Int -> DiatonicInterval
compoundFifth o     = DiatonicInterval { dia_interval_type   = FIFTH
                                       , dia_interval_octave = o }

compoundSixth       :: Int -> DiatonicInterval
compoundSixth o     = DiatonicInterval { dia_interval_type   = SIXTH
                                       , dia_interval_octave = o }

compoundSeventh     :: Int -> DiatonicInterval
compoundSeventh o   = DiatonicInterval { dia_interval_type   = SEVENTH
                                       , dia_interval_octave = o }

