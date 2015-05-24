{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Names.Chord
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Equal temperament chords
--
--------------------------------------------------------------------------------

module Payasan.Base.Names.Chord
  ( 

    major_triad
  , minor_triad

  , no_root
  , no_3
  , no_5
  , no_7
  , no_9
  , no_11
  , no_13

  , dim_5
  , aug_5
  , min_7
  , maj_7
  , dim_7
  , sus_4
  , dim_9
  , min_9
  , maj_9

  -- * Transformation
  , octaveDouble
  , octaveRestrict

  ) where

import Payasan.Base
import Payasan.Base.Names.Interval


import qualified Data.IntMap as IM






major_triad         :: Pitch -> Chord
major_triad root    =  
    Chord { chord_root  = root
          , chord_ivals = IM.fromList [ (1, perfect_unison)
                                      , (3, major_third)
                                      , (5, perfect_fifth)
                                      ]
          }
                         
        
minor_triad         :: Pitch -> Chord
minor_triad root    = 
    Chord { chord_root  = root
          , chord_ivals =  IM.fromList [ (1, perfect_unison)
                                       , (3, minor_third) 
                                       , (5, perfect_fifth)
                                       ]
          }
                                 

no_root             :: Chord -> Chord
no_root             = chordDelete 1

no_3                :: Chord -> Chord
no_3                = chordDelete 3

no_5                :: Chord -> Chord
no_5                = chordDelete 5

no_7                :: Chord -> Chord
no_7                = chordDelete 7

no_9                :: Chord -> Chord
no_9                = chordDelete 9

no_11               :: Chord -> Chord
no_11               = chordDelete 11

no_13               :: Chord -> Chord
no_13               = chordDelete 13
 

dim_5               :: Chord -> Chord
dim_5               = no_5 . chordAdd 5 diminished_fifth

aug_5               :: Chord -> Chord
aug_5               = no_5 . chordAdd 5 augmented_fifth

min_7               :: Chord -> Chord
min_7               = chordAdd 7 minor_seventh

maj_7               :: Chord -> Chord
maj_7               = chordAdd 7 major_seventh


dim_7               :: Chord -> Chord
dim_7               = chordAdd 7 diminished_seventh


-- no3 . add_perfect_fourth 
sus_4               :: Chord -> Chord
sus_4               = no_3 . chordAdd 4 perfect_fourth


dim_9               :: Chord -> Chord
dim_9               = chordAdd 7 minor_seventh . chordAdd 9 diminished_ninth


-- Don't implement Pachet's @ninth@ instead implement min9 and maj9

min_9               :: Chord -> Chord
min_9               = chordAdd 7 minor_seventh . chordAdd 9 minor_ninth

maj_9               :: Chord -> Chord
maj_9               = chordAdd 7 major_seventh . chordAdd 9 major_ninth



--------------------------------------------------------------------------------
-- 


octaveDouble :: Chord -> Chord
octaveDouble = chordF dbl
  where
    dbl s = foldr (\i ac -> case IM.lookup i ac of
                              Nothing -> ac
                              Just ivl -> IM.insert (i+12) ivl ac)
                  s
                  [1..12]
                   

octaveRestrict :: Chord -> Chord
octaveRestrict = chordF fn
  where
    fn old = foldr (\i ac -> case IM.lookup i old of
                              Nothing -> ac
                              Just ivl -> IM.insert i ivl ac)
                    IM.empty
                    [1..12]
                  


-- open chords also a transformation