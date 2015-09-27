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

  ) where


import Payasan.Base.ScaleDegree



simple_unison           :: DiatonicInterval
simple_unison           = DiatonicInterval { dinterval_type   = UNISON
                                           , dinterval_octave = 0 }

simple_second           :: DiatonicInterval
simple_second           = DiatonicInterval { dinterval_type   = SECOND
                                           , dinterval_octave = 0 }

simple_third            :: DiatonicInterval
simple_third            = DiatonicInterval { dinterval_type   = THIRD
                                           , dinterval_octave = 0 }

simple_fourth           :: DiatonicInterval
simple_fourth           = DiatonicInterval { dinterval_type   = FOURTH
                                           , dinterval_octave = 0 }

simple_fifth            :: DiatonicInterval
simple_fifth            = DiatonicInterval { dinterval_type   = FIFTH
                                           , dinterval_octave = 0 }

simple_sixth            :: DiatonicInterval
simple_sixth            = DiatonicInterval { dinterval_type   = SIXTH
                                           , dinterval_octave = 0 }

simple_seventh          :: DiatonicInterval
simple_seventh          = DiatonicInterval { dinterval_type   = SEVENTH
                                           , dinterval_octave = 0 }


