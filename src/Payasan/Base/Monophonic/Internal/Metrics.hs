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

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

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
lowestStep ph = fmap snd $ collectP fn Nothing () ph
  where
    fn Nothing        p = (\k -> Just (k,p)) <$> asksLocal local_key 
    fn (Just (k0,p0)) p = (\k1 -> if (k1,p) `ctxIsLower` (k0,p0) 
                                    then Just (k1,p) else Just (k0,p0))
                            <$> asksLocal local_key

highestStep :: Phrase OveScaleStep drn anno -> Maybe OveScaleStep
highestStep ph = fmap snd $ collectP fn Nothing () ph
  where
    fn Nothing        p = (\k -> Just (k,p)) <$> asksLocal local_key 
    fn (Just (k0,p0)) p = (\k1 -> if (k1,p) `ctxIsHigher` (k0,p0) 
                                    then Just (k1,p) else Just (k0,p0))
                            <$> asksLocal local_key
