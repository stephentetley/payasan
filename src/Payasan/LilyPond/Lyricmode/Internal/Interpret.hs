{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Lyricmode.Internal.Interpret
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Lyricmode output.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Lyricmode.Internal.Interpret
  ( 
     extractRhythm
  ) where

import Payasan.LilyPond.Lyricmode.Internal.Base

import Payasan.Score.Elementary.Internal.Syntax
import Payasan.Score.Elementary.Internal.Traversals

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Names.Pitch


-- TODO - need to remove punctuation as well.
--
extractRhythm :: LyricSection1 anno -> Section Pitch Duration anno
extractRhythm = mapPitch (const b_4)

