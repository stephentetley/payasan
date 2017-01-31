{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.Metrics
-- Copyright   :  (c) Stephen Tetley 2015-2017
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

module Payasan.Score.Cadenza.Internal.Metrics
  (

    lowestPitch
  , highestPitch

  , lowestDiatonic
  , highestDiatonic

  , semitoneInterval

  , grossContour

  , refinedContour


  ) where


import Payasan.Score.Cadenza.Internal.Syntax
import Payasan.Score.Cadenza.Internal.Traversals

import Payasan.Score.Analysis.Common

import Payasan.Base.Basis
import Payasan.Base.Pitch
import Payasan.Base.Diatonic


import Control.Monad.State              -- package: mtl

-- Simple metrics


lowestPitch :: Section Pitch drn anno -> Maybe Pitch
lowestPitch = foldPitch fn Nothing
  where
    fn Nothing   p                      = Just p
    fn (Just p0) p | p `isLower` p0     = Just p
                   | otherwise          = Just p0

highestPitch :: Section Pitch drn anno -> Maybe Pitch
highestPitch = foldPitch fn Nothing
  where
    fn Nothing   p                      = Just p
    fn (Just p0) p | p `isHigher` p0    = Just p
                   | otherwise          = Just p0


lowestDiatonic :: Section Diatonic drn anno -> Maybe Diatonic
lowestDiatonic = fmap nubAlteration . foldPitch fn Nothing
  where
    fn Nothing   s = Just s
    fn (Just s0) s = if diatonicIndex s < diatonicIndex s0 then Just s else Just s0


highestDiatonic :: Section Diatonic drn anno -> Maybe Diatonic
highestDiatonic = fmap nubAlteration . foldPitch fn Nothing
  where
    fn Nothing   s = Just s
    fn (Just s0) s = if diatonicIndex s > diatonicIndex s0 then Just s else Just s0



--------------------------------------------------------------------------------
-- Contours


contourAlgo :: (Pitch -> Pitch -> ctour) 
            -> CadenzaPitchAlgo (Maybe Pitch) Pitch ctour
contourAlgo comp = CadenzaPitchAlgo { initial_stateP = Nothing
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


semitoneInterval :: Section Pitch drn anno -> Section Int drn anno
semitoneInterval = transformP (contourAlgo comp)
  where
    comp pold pnew = let sc = interval_semitones $ intervalBetween pold pnew
                     in if pnew `isLower` pold then negate sc else sc





grossContour :: Section Pitch drn anno -> Section GrossContour drn anno
grossContour = transformP (contourAlgo comp)
  where
    comp pold pnew | pnew `isHigher` pold = UP
                   | pnew `isLower`  pold = DOWN
                   | otherwise            = GROSS_SAME





refinedContour :: Section Pitch drn anno -> Section RefinedContour drn anno
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
