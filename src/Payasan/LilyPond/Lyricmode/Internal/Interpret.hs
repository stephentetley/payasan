{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Lyricmode.Internal.Interpret
-- Copyright   :  (c Stephen Tetley 2015
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

import Payasan.Base.Elementary.Internal.Syntax
import Payasan.Base.Elementary.Internal.Traversals

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Names.Pitch


-- TODO - need to remove punctuation as well.
--
extractRhythm :: LyricPhrase1 anno -> Phrase Pitch Duration anno
extractRhythm = mapPitch (const b_4)

