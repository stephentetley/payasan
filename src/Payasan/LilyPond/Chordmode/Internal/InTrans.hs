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

import Payasan.Base.Elementary.Internal.LilyPondInTrans
import Payasan.Base.Elementary.Internal.Traversals
import Payasan.Base.Elementary.Internal.Syntax

import Payasan.Base.Internal.LilyPond.Syntax

translateInput :: LyChordPart -> StdChordPart
translateInput = trafoAnnos . trafoDuration

trafoAnnos :: Part LyPitch drn ChordSuffix -> Part Chord drn ()
trafoAnnos = mapPitchAnno $ \p a -> (Chord (toPitchAbs p) a, ())