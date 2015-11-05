{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Csound.Syntax
-- Copyright   :  (c) Stephen Tetley 2015
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

module Payasan.Base.Internal.Csound.Syntax
  ( 

    CsPhrase
  , CsBar
  , CsNoteGroup
  , CsElement
  , CsNote

  , CsPhrase1
  , CsBar1
  , CsNoteGroup1
  , CsElement1


  , CpsPitch(..)

  , middle_c

{-
  , toPitch
  , fromPitch

  , toDuration
  , fromDuration
-}

  ) where

import Payasan.Base.Internal.Base
import qualified Payasan.Base.Internal.BeamSyntax as BEAM


import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data
import Data.Fixed

--------------------------------------------------------------------------------
-- Syntax

-- | TODO - annos potentially allow expressive variation in 
-- notelist output.

type CsPhrase                   = CsPhrase1     ()
type CsBar                      = CsBar1        ()      
type CsNoteGroup                = CsNoteGroup1  ()
type CsElement                  = CsElement1    ()
type CsNote                     = BEAM.Note     CpsPitch Seconds


type CsPhrase1 anno             = BEAM.Phrase     CpsPitch Seconds anno
type CsBar1 anno                = BEAM.Bar        CpsPitch Seconds anno
type CsNoteGroup1 anno          = BEAM.NoteGroup  CpsPitch Seconds anno
type CsElement1 anno            = BEAM.Element    CpsPitch Seconds anno


-- Just give Pitch and NoteLength Cs- prefix as they are 
-- likely to otherwise cause confusion in debug output and 
-- conversion code.


-- Use CpsPch - we can always print it as Hz if prefered.
-- 
data CpsPitch = CpsPitch Milli
  deriving (Data,Eq,Ord,Show,Typeable)





middle_c :: CpsPitch
middle_c = CpsPitch 8.000




--------------------------------------------------------------------------------
-- Pretty instances are for debugging.

instance Pretty CpsPitch where 
  pPrint (CpsPitch a)   = text $ show a



