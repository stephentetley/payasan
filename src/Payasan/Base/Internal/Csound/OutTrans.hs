{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Csound.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Pitch to CpsPitch.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Csound.OutTrans
  ( 
    translateToCsoundP
  ) where


import Payasan.Base.Internal.Csound.Syntax

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals

import Payasan.Base.Pitch


translateToCsoundP :: Phrase Pitch drn anno -> Phrase CpsPitch drn anno
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
elementP (Chord ps d a t)       = Chord (map toCpsPitch ps) d a t
elementP (Graces ns)            = Graces $ map noteP ns
elementP (Punctuation s)        = Punctuation s


noteP :: Note Pitch drn -> Note CpsPitch drn
noteP (Note pch drn)            = Note (toCpsPitch pch) drn

