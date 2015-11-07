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
-- Convert Pitch to CpsPitch and Duration to Seconds.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Csound.OutTrans
  ( 
    translateToCsoundPD
  ) where


import Payasan.Base.Internal.Csound.Syntax

import Payasan.Base.Internal.Base
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import Payasan.Base.Pitch


translateToCsoundPD :: Phrase Pitch Duration anno -> Phrase CpsPitch Seconds anno
translateToCsoundPD = transformD drn_algo . transformP pch_algo



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


--------------------------------------------------------------------------------
-- Duration translation


drn_algo :: BeamDurationAlgo () Duration Seconds
drn_algo = BeamDurationAlgo
    { initial_stateD    = ()
    , element_trafoD    = elementD
    }


elementD :: Element pch Duration anno -> Mon () (Element pch Seconds anno)
elementD (NoteElem e a t)       = (\e1 -> NoteElem e1 a t) <$> noteD e
elementD (Rest d)               = withBPM $ \bpm -> Rest (toSeconds bpm d)
elementD (Spacer d)             = withBPM $ \bpm -> Spacer (toSeconds bpm d)
elementD (Skip d)               = withBPM $ \bpm -> Skip (toSeconds bpm d)

elementD (Chord ps d a t)       = 
    withBPM $ \bpm -> Chord ps (toSeconds bpm d) a t

elementD (Graces ns)            = Graces <$> mapM noteD ns
elementD (Punctuation s)        = pure $ Punctuation s


noteD :: Note pch Duration -> Mon () (Note pch Seconds)
noteD (Note pch drn)            = withBPM $ \bpm -> Note pch $ toSeconds bpm drn


withBPM :: (BPM -> a) -> Mon () a
withBPM f = f <$> asks local_bpm
