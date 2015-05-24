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
  , perfect_eighth
  , perfect_octave

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


import Payasan.Base


perfect_unison          :: Interval
perfect_unison          = 0

augmented_unison        :: Interval
augmented_unison        = 1

diminished_second       :: Interval
diminished_second       = 0

minor_second            :: Interval
minor_second            = 1

major_second            :: Interval
major_second            = 2

augmented_second        :: Interval
augmented_second        = 3

diminished_third        :: Interval
diminished_third        = 2

minor_third             :: Interval
minor_third             = 3

major_third             :: Interval
major_third             = 4

augmented_third         :: Interval
augmented_third         = 5

diminished_fourth       :: Interval
diminished_fourth       = 4

perfect_fourth          :: Interval
perfect_fourth          = 5

augmented_fourth        :: Interval
augmented_fourth        = 6

diminished_fifth        :: Interval
diminished_fifth        = 6

perfect_fifth           :: Interval
perfect_fifth           = 7

augmented_fifth         :: Interval
augmented_fifth         = 8

diminished_sixth        :: Interval
diminished_sixth        = 7

minor_sixth             :: Interval 
minor_sixth             = 8

major_sixth             :: Interval 
major_sixth             = 9

augmented_sixth         :: Interval
augmented_sixth         = 10

diminished_seventh      :: Interval
diminished_seventh      = 9

minor_seventh           :: Interval
minor_seventh           = 10

major_seventh           :: Interval 
major_seventh           = 11

augmented_seventh       :: Interval
augmented_seventh       = 12

diminished_octave       :: Interval
diminished_octave       = 11

perfect_eighth          :: Interval
perfect_eighth          = 12

perfect_octave          :: Interval
perfect_octave          = 12

-- ove

diminished_ninth        :: Interval
diminished_ninth        = 12

minor_ninth             :: Interval 
minor_ninth             = 13

major_ninth             :: Interval 
major_ninth             = 14

augmented_ninth         :: Interval
augmented_ninth         = 15

diminished_tenth        :: Interval
diminished_tenth        = 14

minor_tenth             :: Interval 
minor_tenth             = 15

major_tenth             :: Interval 
major_tenth             = 16

augmented_tenth         :: Interval
augmented_tenth         = 17

diminished_eleventh     :: Interval
diminished_eleventh     = 16

perfect_eleventh        :: Interval
perfect_eleventh        = 17

augmented_eleventh      :: Interval 
augmented_eleventh      = 18

diminished_twelfth      :: Interval
diminished_twelfth      = 18

perfect_twelfth         :: Interval
perfect_twelfth         = 19

augmented_twelfth       :: Interval
augmented_twelfth       = 20 
