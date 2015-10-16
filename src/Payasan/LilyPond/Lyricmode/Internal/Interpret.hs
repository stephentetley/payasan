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

import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Monophonic.Internal.Traversals

import Payasan.Base.Duration
import Payasan.Base.Pitch


-- TODO - need to remove punctuation as well.
--
extractRhythm :: StdLyricPhrase -> Phrase Pitch Duration ()
extractRhythm = mapPitch (const middle_c)