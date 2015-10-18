{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.InTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert ABC to Main Syntax, plus /pushing/ render info
-- into bars as this cannot be done during parsing / 
-- quasiquoting.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABC.InTrans
  (
    translateFromInput
  ) where



import Payasan.Base.Internal.ABC.Syntax

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad
import Payasan.Base.Internal.Scale

import Payasan.Base.Duration
import Payasan.Base.Pitch


translateFromInput :: Phrase ABCPitch ABCNoteLength anno 
                   -> Phrase Pitch Duration anno
translateFromInput = transformP pch_algo . transformD drn_algo

type PTMon a = Mon () a
type DTMon a = Mon UnitNoteLength a

--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: BeamPitchAlgo () ABCPitch Pitch
pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = elementP
    }



elementP :: Element ABCPitch drn anno -> PTMon (Element Pitch drn anno)
elementP (NoteElem e a t m)     = (\e1 -> NoteElem e1 a t m)  <$> noteP e
elementP (Rest d)               = pure $ Rest d
elementP (Skip d)               = pure $ Skip d
elementP (Chord ps d a t m)     = 
    (\ps1 -> Chord ps1 d a t m) <$> mapM transPch ps

elementP (Graces ns)            = Graces    <$> mapM noteP ns
elementP (Punctuation s)        = pure $ Punctuation s


noteP :: Note ABCPitch drn -> PTMon (Note Pitch drn)
noteP (Note pch drn)            = (\p -> Note p drn) <$> transPch pch



-- Pitches might be /natural/ in the score when the are
-- actually sharpened or flattened according to key 
-- signature

transPch :: ABCPitch -> PTMon Pitch
transPch p0 = (\k -> toPitch (buildScale k) p0) <$> asksLocal local_key



--------------------------------------------------------------------------------
-- Translate duration

drn_algo :: BeamDurationAlgo UnitNoteLength ABCNoteLength Duration
drn_algo = BeamDurationAlgo
    { initial_stateD    = UNIT_NOTE_8
    , element_trafoD    = elementD
    }


elementD :: Element pch ABCNoteLength anno -> DTMon (Element pch Duration anno)
elementD (NoteElem e a t m)     = (\e1 -> NoteElem e1 a t m) <$> noteD e
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Skip d)               = Skip      <$> changeDrn d
elementD (Chord ps d a t m)     = (\d1 -> Chord ps d1 a t m) <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns
elementD (Punctuation s)        = pure $ Punctuation s


noteD :: Note pch ABCNoteLength -> DTMon (Note pch Duration)
noteD (Note pch drn)            = Note pch <$> changeDrn drn


changeDrn :: ABCNoteLength -> DTMon Duration
changeDrn d                     = 
    (\unl -> toDuration unl d) <$> asksLocal local_unit_note_len


