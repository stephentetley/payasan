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
    translate
  ) where



import Payasan.Base.Internal.ABC.Syntax
import Payasan.Base.Internal.ABC.Utils

import Payasan.Base.Internal.BeamDurationTrafo as D
import Payasan.Base.Internal.BeamPitchTrafo as P
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as PCH


translate :: Phrase Pitch NoteLength -> Phrase PCH.Pitch Duration
translate = P.transform pch_algo . D.transform drn_algo

type PTMon a = D.Mon () a
type DTMon a = D.Mon UnitNoteLength a

--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: P.BeamPitchAlgo () Pitch PCH.Pitch
pch_algo = P.BeamPitchAlgo
    { P.initial_state           = ()
    , P.bar_info_action         = actionInfoP
    , P.element_trafo           = elementP
    }


actionInfoP :: LocalRenderInfo -> PTMon ()
actionInfoP _ = return ()

elementP :: Element Pitch drn -> PTMon (Element PCH.Pitch drn)
elementP (NoteElem a)           = NoteElem  <$> noteP a
elementP (Rest d)               = pure $ Rest d
elementP (Chord ps d)           = (Chord `flip` d) <$> mapM transPch ps
elementP (Graces ns)            = Graces    <$> mapM noteP ns


noteP :: Note Pitch drn -> PTMon (Note PCH.Pitch drn)
noteP (Note pch drn)            = (\p -> Note p drn) <$> transPch pch

-- likely to change wrt key sig...
transPch :: Pitch -> PTMon PCH.Pitch
transPch = pure . toPitch



--------------------------------------------------------------------------------
-- Translate duration

drn_algo :: D.BeamDurationAlgo UnitNoteLength NoteLength Duration
drn_algo = D.BeamDurationAlgo
    { D.initial_state           = UNIT_NOTE_8
    , D.bar_info_action         = actionInfoD
    , D.element_trafo           = elementD
    }

actionInfoD :: LocalRenderInfo -> DTMon ()
actionInfoD info = put (local_unit_note_len info)

elementD :: Element pch NoteLength -> DTMon (Element pch Duration)
elementD (NoteElem a)           = NoteElem  <$> noteD a
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Chord ps d)           = Chord ps  <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns


noteD :: Note pch NoteLength -> DTMon (Note pch Duration)
noteD (Note pch drn)            = Note pch <$> changeDrn drn


changeDrn :: NoteLength -> DTMon Duration
changeDrn d                     = (durationT `flip` d) <$> get


durationT :: UnitNoteLength -> NoteLength -> Duration
durationT unl d = 
    let rat = rduration unl d in case rationalToDuration rat of
      Nothing -> dLonga
      Just ans -> ans

