{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Csound.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Pitch to CpsPitch.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Csound.OutTrans
  ( 
    translateToCsoundP
  ) where


import Payasan.PSC.Csound.Syntax

import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.Base.AltPitch
import Payasan.Base.Pitch


translateToCsoundP :: Part Pitch drn anno -> Part CpsPitch drn anno
translateToCsoundP = mapPitch pitchToCpsPitch

