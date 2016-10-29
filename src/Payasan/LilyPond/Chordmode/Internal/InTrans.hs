{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Chordmode.Internal.InTrans
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translate input.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Chordmode.Internal.InTrans
  ( 
    translateInput      -- name problem
  ) where

import Payasan.LilyPond.Chordmode.Internal.Base

import Payasan.Score.Elementary.Internal.LilyPondInTrans
import Payasan.Score.Elementary.Internal.Traversals
import Payasan.Score.Elementary.Internal.Syntax

import Payasan.PSC.Backend.LilyPond.Syntax

translateInput :: LyChordPart -> StdChordPart
translateInput = trafoAnnos . trafoDuration

trafoAnnos :: Part LyPitch drn ChordSuffix -> Part Chord drn ()
trafoAnnos = mapPitchAnno $ \p a -> (Chord (toPitchAbs p) a, ())