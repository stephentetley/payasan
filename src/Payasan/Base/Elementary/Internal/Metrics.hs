{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.Metrics
-- Copyright   :  (c) Stephen Tetley 2015
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

import Payasan.Base.Internal.Base
import Payasan.Base.Internal.Contour
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Pitch
import Payasan.Base.ScaleDegree



-- Simple metrics

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
            -> MonoPitchAlgo (Maybe Pitch) Pitch ctour
contourAlgo comp = MonoPitchAlgo { initial_stateP = Nothing
                                 , element_trafoP = fn }
  where   
    fn (Note p d a t)   = do { opt <- get 
                             ; case opt of 
                                  Nothing -> put (Just p) >> return (Rest d)
                                  Just p0 -> 
                                     let ct = comp p0 p
                                     in put (Just p) >> return (Note ct d a t)
                             }

    fn (Rest d)         = pure $ Rest d
    fn (Spacer d)       = pure $ Spacer d
    fn (Skip d)         = pure $ Skip d
    fn (Punctuation s)  = pure $ Punctuation s


semitoneInterval :: forall drn anno. 
                    Phrase Pitch drn anno -> Phrase Int drn anno
semitoneInterval = transformP (contourAlgo comp)
  where
    comp pold pnew = let sc = interval_semitones $ intervalBetween pold pnew
                     in if pnew `isLower` pold then negate sc else sc




grossContour :: forall drn anno. 
                Phrase Pitch drn anno -> Phrase GrossContour drn anno
grossContour = transformP (contourAlgo comp)
  where
    comp pold pnew | pnew `isHigher` pold = UP
                   | pnew `isLower`  pold = DOWN
                   | otherwise            = GROSS_SAME




refinedContour :: forall drn anno. 
                  Phrase Pitch drn anno -> Phrase RefinedContour drn anno
refinedContour = transformP (contourAlgo comp)
  where
    comp pold pnew 
        | pnew `isHigher` pold = let ival = intervalBetween pold pnew
                                 in if interval_distance ival > 2 
                                    then LEAP_UP else STEP_UP

        | pnew `isLower`  pold = let ival = intervalBetween pnew pold
                                 in if interval_distance ival > 2 
                                    then LEAP_DOWN else STEP_DOWN

        | otherwise            = REFINED_SAME
