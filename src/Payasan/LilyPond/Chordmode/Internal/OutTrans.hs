{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Chordmode.Internal.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015
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

import Payasan.Base.Elementary.Internal.Traversals
import Payasan.Base.Elementary.Internal.Syntax as ELEM

import Payasan.Base.Internal.LilyPond.Syntax

translateOutput :: ELEM.Phrase Chord drn anno -> ELEM.Phrase LyPitch drn ChordSuffix
translateOutput = trafoAnnos 

trafoAnnos :: ELEM.Phrase Chord drn anno -> ELEM.Phrase LyPitch drn ChordSuffix
trafoAnnos = mapPitchAnno $ \ch _ -> (fromPitchAbs $ chord_root ch, chord_suffix ch)