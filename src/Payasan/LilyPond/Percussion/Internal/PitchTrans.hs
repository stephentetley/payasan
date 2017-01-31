{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Percussion.Internal.PitchTrans
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert DrumPitch to MidiPitch.
-- 
--------------------------------------------------------------------------------

module Payasan.LilyPond.Percussion.Internal.PitchTrans
  ( 
    translate
  ) where

import Payasan.LilyPond.Percussion.Internal.Base

import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.Base.AltPitch (MidiPitch)
import Payasan.Base.Duration


translate :: Part DrumPitch Duration anno -> Part MidiPitch Duration anno
translate = transformExternal pch_algo


type PTMon   a      = Mon () a


--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: ExternalAlgo () DrumPitch MidiPitch drn drn anno anno
pch_algo = ExternalAlgo
    { initial_state     = ()
    , element_trafo     = elementP
    }


elementP :: Element DrumPitch drn anno -> PTMon (Element MidiPitch drn anno)
elementP (Note p d a t)         = (\p1 -> Note p1 d a t) <$> pure (toMidiValue p)
elementP (Rest d)               = pure $ Rest d
elementP (Spacer d)             = pure $ Spacer d
elementP (Skip d)               = pure $ Skip d
elementP (Chord ps d a t)       = pure $ Chord (map toMidiValue ps) d a t
elementP (Graces ns)            = Graces <$> mapM grace1P ns
elementP (Punctuation s)        = pure $ Punctuation s


grace1P :: Grace1 DrumPitch drn -> PTMon (Grace1 MidiPitch drn)
grace1P (Grace1 p d)            = pure $ Grace1 (toMidiValue p) d
