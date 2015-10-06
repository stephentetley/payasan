{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Chordmode.Internal.InTrans
-- Copyright   :  (c) Stephen Tetley 2015
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

import Payasan.Base.Monophonic.Internal.LilyPondInTrans
import Payasan.Base.Monophonic.Internal.Traversals
import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.LilyPond.Syntax

translateInput :: LyChordPhrase -> StdChordPhrase
translateInput = trafoAnnos . trafoDuration

trafoAnnos :: Phrase LyPitch drn ChordSuffix -> Phrase Chord drn ()
trafoAnnos = mapPitchAnno $ \p a -> (Chord (toPitchAbs p) a, ())