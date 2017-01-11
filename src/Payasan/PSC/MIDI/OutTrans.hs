{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.MIDI.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Pitch to MidiPitch and Duration to RDuration.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.MIDI.OutTrans
  ( 
    translateToMidiP
  ) where


import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.Base.AltPitch
import Payasan.Base.Pitch


translateToMidiP :: Part Pitch drn anno -> Part MidiPitch drn anno
translateToMidiP = mapPitch pitchToMidiPitch



-- No need for a Duration translation. This is done together with
-- coalescing ties.

