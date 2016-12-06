{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Csound.Syntax
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist segmented into bars, with notes, rests, 
-- chords, grace notes and triplets.
--
-- Concrete syntax following Csound notelists.
--
-- This module is old and in the process of being superceded.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Csound.Syntax
  ( 

    CsoundNoteStream

  , CpsPitch
  , Seconds             -- re-export

  , cpsPitchValue 

  ) where

import Payasan.PSC.Csound.Base

import Payasan.PSC.Repr.External.Syntax

import Payasan.Base.AltPitch
import Payasan.Base.Basis


--------------------------------------------------------------------------------
-- Syntax


type CsoundNoteStream anno = [Element CpsPitch Seconds anno]





-- TODO - Probably should have a CPitch constructor in Value to 
-- ensure nice printing...
--
cpsPitchValue :: CpsPitch -> Value
cpsPitchValue = VCpsPitch . getCpsPitch





