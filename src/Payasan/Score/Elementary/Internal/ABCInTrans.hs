{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.ABCInTrans
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert ABC to Elementary syntax.
--
--------------------------------------------------------------------------------

module Payasan.Score.Elementary.Internal.ABCInTrans
  (
    abcTranslate
  ) where



import Payasan.Score.Elementary.Internal.Syntax
import Payasan.Score.Elementary.Internal.Traversals

import Payasan.PSC.Base.ABCCommon 
import Payasan.PSC.Base.RewriteMonad
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Scale


abcTranslate :: ABCElemPart -> Part Pitch Duration ()
abcTranslate = transformP pch_algo . transformD drn_algo


--------------------------------------------------------------------------------
-- Pitch translation

type PMon a = Mon () a

pch_algo :: ElemPitchAlgo () ABCPitch Pitch
pch_algo = ElemPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = elementP
    }


elementP :: Element ABCPitch drn anno -> PMon (Element Pitch drn anno) 
elementP (Note p d a t)         = (\p1 -> Note p1 d a t) <$> transPch p
elementP (Rest d)               = pure $ Rest d
elementP (Spacer d)             = pure $ Spacer d
elementP (Skip d)               = pure $ Skip d
elementP (Punctuation s)        = pure $ Punctuation s



transPch :: ABCPitch -> PMon Pitch
transPch p0 = (\k -> toPitch (buildScale k) p0) <$> asks section_key

--------------------------------------------------------------------------------
-- Translate duration

type DMon a = Mon UnitNoteLength a

drn_algo :: ElemDurationAlgo UnitNoteLength ABCNoteLength Duration
drn_algo = ElemDurationAlgo
    { initial_stateD    = UNIT_NOTE_8
    , element_trafoD    = elementD
    }


elementD :: Element pch ABCNoteLength anno -> DMon (Element pch Duration anno)
elementD (Note p d a t)         = (\d1 -> Note p d1 a t) <$> changeDuration d
elementD (Rest d)               = Rest    <$> changeDuration d
elementD (Spacer d)             = Spacer  <$> changeDuration d
elementD (Skip d)               = Skip    <$> changeDuration d
elementD (Punctuation s)        = pure $ Punctuation s


changeDuration :: ABCNoteLength -> DMon Duration
changeDuration d = (\unl -> toDuration unl d) <$> asks section_unit_note_len

