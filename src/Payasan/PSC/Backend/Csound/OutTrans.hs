{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Backend.Csound.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Pitch to CpsPitch.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Backend.Csound.OutTrans
  ( 
    translateToCsoundP
  ) where


import Payasan.PSC.Backend.Csound.Syntax

import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.Base.AltPitch
import Payasan.Base.Pitch


translateToCsoundP :: Part Pitch drn anno -> Part CpsPitch drn anno
translateToCsoundP = transformP pch_algo



--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: BeamPitchAlgo () Pitch CpsPitch
pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = liftElementTrafo elementP
    }


elementP :: Element Pitch drn anno -> Element CpsPitch drn anno
elementP (NoteElem e a t)       = NoteElem (noteP e) a t
elementP (Rest d)               = Rest d
elementP (Spacer d)             = Spacer d
elementP (Skip d)               = Skip d
elementP (Chord ps d a t)       = Chord (map pitchToCpsPitch ps) d a t
elementP (Graces ns)            = Graces $ map noteP ns
elementP (Punctuation s)        = Punctuation s


noteP :: Note Pitch drn -> Note CpsPitch drn
noteP (Note pch drn)            = Note (pitchToCpsPitch pch) drn

