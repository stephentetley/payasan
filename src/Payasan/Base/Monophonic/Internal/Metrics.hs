{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.Metrics
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Metrics  - barcount, lowest pitch, highest pitch...
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.Metrics
  (
    barCount
  , lowestPitch
  , highestPitch

  , lowestStep
  , highestStep

  ) where


import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Monophonic.Internal.Traversals

import Payasan.Base.Internal.Base

import Payasan.Base.Pitch
import Payasan.Base.ScaleDegree

barCount :: Phrase pch drn anno -> Int
barCount (Phrase { phrase_bars = bs }) = length bs


             


lowestPitch :: Phrase Pitch drn anno -> Maybe Pitch
lowestPitch = foldPitch fn Nothing
  where
    fn Nothing   p                      = Just p
    fn (Just p0) p | p `isLower` p0     = Just p
                   | otherwise          = Just p0

highestPitch :: Phrase Pitch drn anno -> Maybe Pitch
highestPitch = foldPitch fn Nothing
  where
    fn Nothing   p                      = Just p
    fn (Just p0) p | p `isHigher` p0    = Just p
                   | otherwise          = Just p0


lowestStep :: Phrase OveScaleStep drn anno -> Maybe OveScaleStep
lowestStep = foldPitch fn Nothing
  where
    fn Nothing   s                      = Just s
    fn (Just s0) s | s `isLower` s0     = Just s
                   | otherwise          = Just s0


highestStep :: Phrase OveScaleStep drn anno -> Maybe OveScaleStep
highestStep = foldPitch fn Nothing
  where
    fn Nothing   s                      = Just s
    fn (Just s0) s | s `isHigher` s0    = Just s
                   | otherwise          = Just s0
