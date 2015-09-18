{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Chordmode.Internal.InTrans
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

module Payasan.Chordmode.Internal.InTrans
  ( 
    translateInput      -- name problem
  ) where

import Payasan.Chordmode.Internal.Base
import Payasan.Base.Monophonic.Internal.LilyPondInTrans
import Payasan.Base.Monophonic.Internal.MonoPitchAnnoTrafo
import Payasan.Base.Monophonic.Internal.Syntax

import qualified Payasan.Base.Internal.LilyPond.Syntax as LY
import Payasan.Base.Internal.LilyPond.Utils

translateInput :: LyChordPhrase -> StdChordPhrase
translateInput = trafoAnnos . trafoDuration

trafoAnnos :: Phrase LY.Pitch drn ChordSuffix -> Phrase Chord drn ()
trafoAnnos = mapPitchAnno $ \p a -> (Chord (toPitchAbs p) a, ())