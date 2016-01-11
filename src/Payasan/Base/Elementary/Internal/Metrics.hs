{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.Metrics
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Metrics  - barcount, lowest pitch, highest pitch...
--
-- Contours.
--
--------------------------------------------------------------------------------

module Payasan.Base.Elementary.Internal.Metrics
  (

    barCount
  , pitchHisto
  , pitchNameHisto
  , octaveHisto


  , lowestPitch
  , highestPitch

  , lowestStep
  , highestStep

  , semitoneInterval

  , grossContour

  , refinedContour


  ) where


import Payasan.Base.Elementary.Internal.Syntax
import Payasan.Base.Elementary.Internal.Traversals

import Payasan.Base.Internal.AnalysisCommon
import Payasan.Base.Internal.AnalysisTrace
import Payasan.Base.Internal.Base
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Pitch
import Payasan.Base.ScaleDegree






-- Simple metrics

barCount :: Phrase pch drn anno -> Int
barCount (Phrase { phrase_bars = bs }) = length bs




-- histograms 

pitchHisto :: Phrase Pitch drn anno -> Histogram Pitch
pitchHisto = foldPitch fn empty
  where
    fn histo p = incr p histo


pitchNameHisto :: Phrase Pitch drn anno -> Histogram PitchName
pitchNameHisto = foldPitch fn empty
  where
    fn histo p = incr (pitch_name p) histo


octaveHisto :: Phrase Pitch drn anno -> Histogram Int
octaveHisto = foldPitch fn empty
  where
    fn histo p = incr (pitch_octave p) histo



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


lowestStep :: Phrase ChromaticPitch drn anno -> Maybe DiatonicPitch
lowestStep = fmap diatonic_base . foldPitch fn Nothing
  where
    fn Nothing   s                      = Just s
    fn (Just s0) s | s `isLower` s0     = Just s
                   | otherwise          = Just s0


highestStep :: Phrase ChromaticPitch drn anno -> Maybe DiatonicPitch
highestStep = fmap diatonic_base . foldPitch fn Nothing
  where
    fn Nothing   s                      = Just s
    fn (Just s0) s | s `isHigher` s0    = Just s
                   | otherwise          = Just s0



--------------------------------------------------------------------------------
-- Contours


contourAlgo :: (Pitch -> Pitch -> ctour) 
             -> TraceAlgo (Maybe Pitch) Pitch drn anno ctour
contourAlgo comp = TraceAlgo { initial_trace_state = Nothing
                             , element_trace_trafo = fn }
  where   
    fn (Note p _ _ _)   = do { opt <- get 
                             ; case opt of 
                                  Nothing -> put (Just p) >> return Blank
                                  Just p0 -> 
                                     let ct = comp p0 p
                                     in put (Just p) >> return (Element ct)
                             }

    fn (Rest {})        = pure $ Blank
    fn (Spacer {})      = pure $ Blank
    fn (Skip {})        = pure $ Blank
    fn (Punctuation {}) = pure $ Blank



semitoneInterval :: forall drn anno. 
                    Phrase Pitch drn anno -> TracePhrase Int
semitoneInterval = trace (contourAlgo comp)
  where
    comp pold pnew = let sc = interval_semitones $ intervalBetween pold pnew
                     in if pnew `isLower` pold then negate sc else sc


grossContour :: forall drn anno. 
                Phrase Pitch drn anno -> TracePhrase GrossContour
grossContour = trace (contourAlgo comp)
  where
    comp pold pnew | pnew `isHigher` pold = UP
                   | pnew `isLower`  pold = DOWN
                   | otherwise            = GROSS_SAME




refinedContour :: forall drn anno. 
                  Phrase Pitch drn anno -> TracePhrase RefinedContour
refinedContour = trace (contourAlgo comp)
  where
    comp pold pnew 
        | pnew `isHigher` pold = let ival = intervalBetween pold pnew
                                 in if interval_distance ival > 2 
                                    then LEAP_UP else STEP_UP

        | pnew `isLower`  pold = let ival = intervalBetween pnew pold
                                 in if interval_distance ival > 2 
                                    then LEAP_DOWN else STEP_DOWN

        | otherwise            = REFINED_SAME


