{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Chordmode.Internal.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- (Pipeline)
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Chordmode.Internal.OutTrans
  ( 
    translateOutput      -- name problem
  ) where

import Payasan.LilyPond.Chordmode.Internal.Base

import Payasan.Score.Elementary.Internal.Traversals
import Payasan.Score.Elementary.Internal.Syntax         as ELEM

import Payasan.PSC.LilyPond.Base

translateOutput :: ELEM.Section Chord drn anno -> ELEM.Section LyPitch drn ChordSuffix
translateOutput = trafoAnnos 

trafoAnnos :: ELEM.Section Chord drn anno -> ELEM.Section LyPitch drn ChordSuffix
trafoAnnos = mapPitchAnno $ \ch _ -> (fromPitchAbs $ chord_root ch, chord_suffix ch)