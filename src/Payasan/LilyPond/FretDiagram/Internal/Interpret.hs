{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Internal.Interpret
-- Copyright   :  (c) Stephen Tetley 2015-2016
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

import Payasan.Base.Internal.SyntaxCommon

import Payasan.Base.Names.DiatonicInterval
import Payasan.Base.Pitch
import Payasan.Base.Diatonic


import Data.Maybe



-- | Needs Key for pitch spelling.
--
interpretFretDiagram :: Key -> GuitarTuning -> FretDiagram -> [Pitch]
interpretFretDiagram key gt fb = 
    applyIntervals key gt $ maybeIntervals fb


maybeIntervals :: FretDiagram -> [Maybe DiatonicInterval]
maybeIntervals = map fn . fd_fingerings
  where
    fn (Fingering _ x) = case x of
                           OPEN -> Just simple_unison
                           MUTED -> Nothing
                           FretNumber n -> Just $ toDiatonicInterval $  n + 1


applyIntervals :: Key -> GuitarTuning -> [Maybe DiatonicInterval] -> [Pitch]
applyIntervals key gt ivs = catMaybes $ zipWith fn ivs gt
  where
    fn :: Maybe DiatonicInterval -> Pitch -> Maybe Pitch
    fn mb p = fmap (\ivl -> transposeDiatonically key ivl p) mb


