{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Internal.Interpret
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Fret diagrams parser.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.FretDiagram.Internal.Interpret
  ( 
     
    interpretFretDiagram

  ) where

import Payasan.LilyPond.FretDiagram.Internal.Base

import Payasan.Base.Internal.CommonSyntax

import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Pitch
import Payasan.Base.ScaleDegree


import Data.Maybe


interpretFretDiagram :: Key -> GuitarTuning -> FretBoard -> [Pitch]
interpretFretDiagram key gt fb = 
    let ivals = fretBoardIntervals fb in applyIntervals key gt ivals

fretBoardIntervals :: FretBoard -> [Maybe DiatonicInterval]
fretBoardIntervals = map fn . fretboard_fingerings
  where
    fn (Fingering _ x) = case x of
                           OPEN -> Just simple_unison
                           MUTED -> Nothing
                           FretNumber n -> Just $ toDiatonicInterval $  n + 1

applyIntervals :: Key -> GuitarTuning -> [Maybe DiatonicInterval] -> [Pitch]
applyIntervals key gt ivs = catMaybes $ zipWith fn ivs gt
  where
    fn :: Maybe DiatonicInterval -> Pitch -> Maybe Pitch
    fn mb p = fmap (\ivl -> transposeWithDiatonicInterval key ivl p) mb