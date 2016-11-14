{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Backend.Csound.Syntax
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
--------------------------------------------------------------------------------

module Payasan.PSC.Backend.Csound.Syntax
  ( 

    CsoundNoteStream

  , CpsPitch
  , Seconds             -- re-export

  , cpsPitchValue 

  ) where

import Payasan.PSC.Backend.Csound.IStmt

import Payasan.PSC.Repr.External.Syntax

import Payasan.Base.AltPitch

import Data.Data
import Data.Fixed

--------------------------------------------------------------------------------
-- Syntax


type CsoundNoteStream anno = [Element CpsPitch Seconds anno]





-- TODO - Probably should have a CPitch constructor in Value to 
-- ensure nice printing...
--
cpsPitchValue :: CpsPitch -> Value
cpsPitchValue = VCpsPitch . getCpsPitch





