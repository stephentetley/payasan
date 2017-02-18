{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.ABC.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert pitch and duration to their ABC equivalents prior 
-- to printing.
--
--------------------------------------------------------------------------------

module Payasan.PSC.ABC.OutTrans
  (
    translateToABCPartOut
  ) where


import Payasan.PSC.Base.ABCCommon

import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Scale




translateToABCPartOut :: Part Pitch Duration anno -> Part ABCPitch ABCNoteLength anno
translateToABCPartOut = 
    transformExternal pch_algo . transformExternal drn_algo


--------------------------------------------------------------------------------
-- Pitch translation



pch_algo :: ExternalAlgo () Pitch ABCPitch drn drn anno anno
pch_algo = ExternalAlgo
    { initial_state     = ()
    , element_trafo     = elementP
    }

type PMon a = Mon () a

elementP :: Element Pitch drn anno -> PMon (Element ABCPitch drn anno)
elementP (Note p d a t)         = (\p1 -> Note p1 d a t) <$> transPch p
elementP (Rest d)               = pure $ Rest d
elementP (Spacer d)             = pure $ Spacer d
elementP (Skip d)               = pure $ Skip d
elementP (Chord ps d a t)       = 
    (\ps1 -> Chord ps1 d a t) <$> mapM transPch ps

elementP (Graces ns)            = Graces    <$> mapM grace1P ns
elementP (Punctuation s)        = pure $ Punctuation s


grace1P :: Grace1 Pitch drn -> PMon (Grace1 ABCPitch drn)
grace1P (Grace1 pch drn)            = (\p -> Grace1 p drn) <$> transPch pch

-- This should be optimized to not make a scale each time!
--
transPch :: Pitch -> PMon ABCPitch
transPch p0 = (\k -> fromPitch (buildScale k) p0) <$> asks section_key

--------------------------------------------------------------------------------
-- Translate duration

drn_algo :: ExternalAlgo UnitNoteLength pch pch Duration ABCNoteLength anno anno
drn_algo = ExternalAlgo
    { initial_state     = UNIT_NOTE_8
    , element_trafo     = elementD
    }


type DMon a = Mon UnitNoteLength a

-- Skip is just a Rest to ABC
--
elementD :: Element pch Duration anno -> DMon (Element pch ABCNoteLength anno)
elementD (Note p d a t)         = (\d1 -> Note p d1 a t) <$> changeDrn d
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Spacer d)             = Spacer    <$> changeDrn d
elementD (Skip d)               = Skip      <$> changeDrn d
elementD (Chord ps d a t)       = (\d1 -> Chord ps d1 a t) <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM grace1D ns
elementD (Punctuation s)        = pure $ Punctuation s


grace1D :: Grace1 pch Duration -> DMon (Grace1 pch ABCNoteLength)
grace1D (Grace1 p d)            = Grace1 p <$> changeDrn d


changeDrn :: Duration -> DMon ABCNoteLength
changeDrn d                     = 
    (\unl -> fromDuration unl d) <$> asks section_unit_note_len


