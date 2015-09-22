{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Chordmode.Internal.Interpret
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Interpret chordmode definitions.
-- 
-- Note - LilyPond step notation is possibly underspecified with
-- respect to pitch spelling. Potentially LilyPond does some 
-- extrainterpretation (e.g. to make diminished 7th chords which 
-- have doubled flattened 7).
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Chordmode.Internal.Interpret
  ( 
   
    buildNotes

  ) where


import Payasan.LilyPond.Chordmode.Internal.Base


import Payasan.Base.Names.Interval
import Payasan.Base.Pitch

import Data.List (delete)
import Data.Maybe (catMaybes)




buildNotes :: Chord -> [Pitch]
buildNotes (Chord { chord_root = r
                  , chord_suffix = suffix }) = case suffix of
    NamedModifier DIM7 -> [ r
                          , r .+^ diminished_third
                          , r .+^ diminished_fifth
                          , r .+^ diminished_seventh -- double bb
                          ]
    NamedModifier m -> buildSteps r $ modifierSteps m
    ChordSteps s -> buildSteps r s

flat :: Int -> Step
flat i = Step i NVE

nat :: Int -> Step
nat i = Step i NO_ALT

sharp :: Int -> Step
sharp i = Step i PVE

fromAddSteps :: [Step] -> Steps
fromAddSteps xs = Steps { additions = xs, removals = [] }

modifierSteps :: ChordModifier -> Steps
modifierSteps MAJ5      = fromAddSteps [ nat 1, nat 3, nat 5 ]
modifierSteps MAJ6      = fromAddSteps [ nat 1, nat 3, nat 5, nat 6 ]
modifierSteps MAJ7      = fromAddSteps [ nat 1, nat 3, nat 5, nat 7 ]
modifierSteps MAJ9      = fromAddSteps [ nat 1, nat 3, nat 5, nat 7, nat 9 ]
modifierSteps MAJ11     = 
    fromAddSteps [ nat 1, nat 3, nat 5, nat 7, nat 9, nat 11 ]

modifierSteps MAJ13     = 
    fromAddSteps [ nat 1, nat 3, nat 5, nat 7, nat 9, nat 11, nat 13 ]

modifierSteps MIN5      = fromAddSteps [ nat 1, flat 3, nat 5 ]
modifierSteps MIN6      = fromAddSteps [ nat 1, flat 3, nat 5, nat 6 ]
modifierSteps MIN7      = fromAddSteps [ nat 1, flat 3, nat 5, flat 7 ]
modifierSteps MIN9      = fromAddSteps [ nat 1, flat 3, nat 5, flat 7, nat 9 ]
modifierSteps MIN11     = 
    fromAddSteps [ nat 1, flat 3, nat 5, flat 7, nat 9, nat 11 ]

modifierSteps MIN13     = 
    fromAddSteps [ nat 1, flat 3, nat 5, flat 7, nat 9, nat 11, nat 13 ]

modifierSteps DIM5      = fromAddSteps [ nat 1, flat 3, flat 5 ]
modifierSteps DIM7      = fromAddSteps []

modifierSteps AUG5      = fromAddSteps [ nat 1, nat 3, sharp 5 ]
modifierSteps AUG7      = fromAddSteps [ nat 1, nat 3, sharp 5, flat 7 ]

modifierSteps DOM7      = fromAddSteps [ nat 1, nat 3, nat 5, flat 7 ]
modifierSteps DOM9      = fromAddSteps [ nat 1, nat 3, nat 5, flat 7, nat 9 ]
modifierSteps DOM11     = 
    fromAddSteps [ nat 1, nat 3, nat 5, flat 7, nat 9, nat 11 ]

modifierSteps DOM13     = 
    fromAddSteps [ nat 1, nat 3, nat 5, flat 7, nat 9, nat 11, nat 13 ]

modifierSteps MM7       = fromAddSteps [ nat 1, flat 3, nat 5, flat 7 ]

modifierSteps SUS       = fromAddSteps [ nat 1, nat 5 ]
modifierSteps SUS2      = fromAddSteps [ nat 1, nat 2, nat 5 ]
modifierSteps SUS4      = fromAddSteps [ nat 1, nat 4, nat 5 ]

modifierSteps NO_MOD    = fromAddSteps [ nat 1, nat 3, nat 5 ]



buildSteps :: Pitch -> Steps -> [Pitch]
buildSteps r (Steps adds rems) = 
    catMaybes $ map (\s -> fmap (r .+^) $ intervalFrom s) steps
  where
    steps = foldr delete adds rems 

intervalFrom :: Step -> Maybe Interval
intervalFrom (Step 1  NO_ALT)   = Just perfect_unison
intervalFrom (Step 3  NVE)      = Just minor_third
intervalFrom (Step 3  NO_ALT)   = Just major_third
intervalFrom (Step 4  NVE)      = Just diminished_fourth
intervalFrom (Step 4  NO_ALT)   = Just perfect_fourth
intervalFrom (Step 4  PVE)      = Just augmented_fourth
intervalFrom (Step 5  NVE)      = Just diminished_fifth
intervalFrom (Step 5  NO_ALT)   = Just perfect_fifth
intervalFrom (Step 5  PVE)      = Just augmented_fifth
intervalFrom (Step 6  NO_ALT)   = Just major_sixth
intervalFrom (Step 7  NVE)      = Just minor_seventh
intervalFrom (Step 7  NO_ALT)   = Just major_seventh
intervalFrom (Step 9  NO_ALT)   = Just major_ninth
intervalFrom (Step 11 NO_ALT)   = Just perfect_eleventh
intervalFrom (Step 13 NVE)      = Just minor_thirteenth
intervalFrom (Step 13 NO_ALT)   = Just major_thirteenth
intervalFrom (Step 15 NO_ALT)   = Just perfect_fifteenth
intervalFrom _                  = Nothing

