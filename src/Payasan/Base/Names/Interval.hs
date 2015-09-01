{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Names.Interval
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Base types include a type to represent quarter-notes / 
-- beats.
--
--------------------------------------------------------------------------------

module Payasan.Base.Names.Interval
  ( 

  -- * Named intervals
    perfect_unison
  , augmented_unison
  , diminished_second
  , minor_second
  , major_second
  , augmented_second
  , diminished_third
  , minor_third
  , major_third
  , augmented_third
  , diminished_fourth
  , perfect_fourth
  , augmented_fourth
  , diminished_fifth
  , perfect_fifth
  , augmented_fifth
  , diminished_sixth
  , minor_sixth
  , major_sixth
  , augmented_sixth
  , diminished_seventh
  , minor_seventh
  , major_seventh
  , augmented_seventh
  , diminished_octave
  , perfect_octave
  , augmented_octave

  , diminished_ninth
  , minor_ninth
  , major_ninth
  , augmented_ninth
  , diminished_tenth
  , minor_tenth
  , major_tenth
  , augmented_tenth
  , diminished_eleventh
  , perfect_eleventh
  , augmented_eleventh
  , diminished_twelfth
  , perfect_twelfth
  , augmented_twelfth

  ) where


import Payasan.Base.Pitch


perfect_unison          :: Interval
perfect_unison          = Interval { interval_arith_dist = 1
                                   , interval_semitones  = 0 }

augmented_unison        :: Interval
augmented_unison        = Interval { interval_arith_dist = 1
                                   , interval_semitones  = 1 }

diminished_second       :: Interval
diminished_second       = Interval { interval_arith_dist = 2
                                   , interval_semitones  = 0 }

minor_second            :: Interval
minor_second            = Interval { interval_arith_dist = 2
                                   , interval_semitones  = 1 }


major_second            :: Interval
major_second            = Interval { interval_arith_dist = 2
                                   , interval_semitones  = 2 }

augmented_second        :: Interval
augmented_second        = Interval { interval_arith_dist = 2
                                   , interval_semitones  = 3 }

diminished_third        :: Interval
diminished_third        = Interval { interval_arith_dist = 3
                                   , interval_semitones  = 2 }

minor_third             :: Interval
minor_third             = Interval { interval_arith_dist = 3
                                   , interval_semitones  = 3 }

major_third             :: Interval
major_third             = Interval { interval_arith_dist = 3
                                   , interval_semitones  = 4 }

augmented_third         :: Interval
augmented_third         = Interval { interval_arith_dist = 3
                                   , interval_semitones  = 5 }

diminished_fourth       :: Interval
diminished_fourth       = Interval { interval_arith_dist = 4
                                   , interval_semitones  = 4 }

perfect_fourth          :: Interval
perfect_fourth          = Interval { interval_arith_dist = 4
                                   , interval_semitones  = 5 }

augmented_fourth        :: Interval
augmented_fourth        = Interval { interval_arith_dist = 4
                                   , interval_semitones  = 6 }

diminished_fifth        :: Interval
diminished_fifth        = Interval { interval_arith_dist = 5
                                   , interval_semitones  = 6 }

perfect_fifth           :: Interval
perfect_fifth           = Interval { interval_arith_dist = 5
                                   , interval_semitones  = 7 }

augmented_fifth         :: Interval
augmented_fifth         = Interval { interval_arith_dist = 5
                                   , interval_semitones  = 8 }

diminished_sixth        :: Interval
diminished_sixth        = Interval { interval_arith_dist = 6
                                   , interval_semitones  = 7 }

minor_sixth             :: Interval 
minor_sixth             = Interval { interval_arith_dist = 6
                                   , interval_semitones  = 8 }

major_sixth             :: Interval 
major_sixth             = Interval { interval_arith_dist = 6
                                   , interval_semitones  = 9 }

augmented_sixth         :: Interval
augmented_sixth         = Interval { interval_arith_dist = 6
                                   , interval_semitones  = 10 }

diminished_seventh      :: Interval
diminished_seventh      = Interval { interval_arith_dist = 7
                                   , interval_semitones  = 9 }

minor_seventh           :: Interval
minor_seventh           = Interval { interval_arith_dist = 7
                                   , interval_semitones  = 10 }

major_seventh           :: Interval 
major_seventh           = Interval { interval_arith_dist = 7
                                   , interval_semitones  = 11 }

augmented_seventh       :: Interval
augmented_seventh       = Interval { interval_arith_dist = 7
                                   , interval_semitones  = 12 }

diminished_octave       :: Interval
diminished_octave       = Interval { interval_arith_dist = 8
                                   , interval_semitones  = 11 }

perfect_octave          :: Interval
perfect_octave          = Interval { interval_arith_dist = 8
                                   , interval_semitones  = 12 }

augmented_octave        :: Interval
augmented_octave        = Interval { interval_arith_dist = 8
                                   , interval_semitones  = 13 }

-- ove

diminished_ninth        :: Interval
diminished_ninth        = Interval { interval_arith_dist = 9
                                   , interval_semitones  = 12 }

minor_ninth             :: Interval 
minor_ninth             = Interval { interval_arith_dist = 9
                                   , interval_semitones  = 13 }

major_ninth             :: Interval 
major_ninth             = Interval { interval_arith_dist = 9
                                   , interval_semitones  = 14 }

augmented_ninth         :: Interval
augmented_ninth         = Interval { interval_arith_dist = 9
                                   , interval_semitones  = 15 }

diminished_tenth        :: Interval
diminished_tenth        = Interval { interval_arith_dist = 10
                                   , interval_semitones  = 14 }

minor_tenth             :: Interval 
minor_tenth             = Interval { interval_arith_dist = 10
                                   , interval_semitones  = 15 }

major_tenth             :: Interval 
major_tenth             = Interval { interval_arith_dist = 10
                                   , interval_semitones  = 16 }

augmented_tenth         :: Interval
augmented_tenth         = Interval { interval_arith_dist = 10
                                   , interval_semitones  = 17 }

diminished_eleventh     :: Interval
diminished_eleventh     = Interval { interval_arith_dist = 11
                                   , interval_semitones  = 16 }

perfect_eleventh        :: Interval
perfect_eleventh        = Interval { interval_arith_dist = 11
                                   , interval_semitones  = 17 }

augmented_eleventh      :: Interval 
augmented_eleventh      = Interval { interval_arith_dist = 11
                                   , interval_semitones  = 18 }

diminished_twelfth      :: Interval
diminished_twelfth      = Interval { interval_arith_dist = 12
                                   , interval_semitones  = 18 }

perfect_twelfth         :: Interval
perfect_twelfth         = Interval { interval_arith_dist = 12
                                   , interval_semitones  = 19 }

augmented_twelfth       :: Interval
augmented_twelfth       = Interval { interval_arith_dist = 12
                                   , interval_semitones  = 20 }

