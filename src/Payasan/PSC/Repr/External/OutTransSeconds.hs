{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.External.OutTransSeconds
-- Copyright   :  (c) Stephen Tetley 2016-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Duration to Seconds.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.External.OutTransSeconds
  (
    transDurationToSeconds
  ) where


import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Basis

    


transDurationToSeconds :: Part pch Duration anno -> Part pch Seconds anno
transDurationToSeconds = genTransform elementM ()
  where
    elementM e = asks section_bpm >>= \bpm -> pure (element bpm e)


element :: BPM -> Element pch Duration anno -> Element pch Seconds anno
element bpm (Note p d a t)         = Note p (durationLength bpm d) a t
element bpm (Rest d)               = Rest $ durationLength bpm d
element bpm (Spacer d)             = Spacer $ durationLength bpm d
element bpm (Skip d)               = Skip $ durationLength bpm d
element bpm (Chord ps d a t)       = Chord ps (durationLength bpm d) a t
element bpm (Graces ns)            = Graces $ map (grace1 bpm) ns
element _   (Punctuation s)        = Punctuation s


grace1 :: BPM -> Grace1 pch Duration -> Grace1 pch Seconds
grace1 bpm (Grace1 pch drn)        = Grace1 pch (durationLength bpm drn)

