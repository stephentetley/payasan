{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.InTrans
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

module Payasan.Base.Monophonic.Internal.InTrans
  (
    abcTranslate
  ) where



import Payasan.Base.Monophonic.Internal.MonoDurationTrafo as D
import Payasan.Base.Monophonic.Internal.MonoPitchTrafo as P
import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.ABC.Syntax (Pitch,NoteLength)
import Payasan.Base.Internal.ABC.Utils
import Payasan.Base.Internal.Utils

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as PCH


abcTranslate :: Phrase Pitch NoteLength -> Phrase PCH.Pitch Duration
abcTranslate = P.transform pch_algo . D.transform drn_algo

type PTMon a = D.Mon () a
type DTMon a = D.Mon UnitNoteLength a

--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: P.MonoPitchAlgo () Pitch PCH.Pitch
pch_algo = P.MonoPitchAlgo
    { P.initial_state           = ()
    , P.bar_info_action         = actionInfoP
    , P.element_trafo           = elementP
    }


actionInfoP :: LocalRenderInfo -> PTMon ()
actionInfoP _ = return ()

elementP :: Element Pitch drn -> PTMon (Element PCH.Pitch drn)
elementP (Note p d)             = (\pch -> Note pch d) <$> transPch p
elementP (Rest d)               = pure $ Rest d


-- likely to change wrt key sig...
transPch :: Pitch -> PTMon PCH.Pitch
transPch = pure . toPitch



--------------------------------------------------------------------------------
-- Translate duration

drn_algo :: D.MonoDurationAlgo UnitNoteLength NoteLength Duration
drn_algo = D.MonoDurationAlgo
    { D.initial_state           = UNIT_NOTE_8
    , D.bar_info_action         = actionInfoD
    , D.element_trafo           = elementD
    }

actionInfoD :: LocalRenderInfo -> DTMon ()
actionInfoD info = put (local_unit_note_len info)

elementD :: Element pch NoteLength -> DTMon (Element pch Duration)
elementD (Note p d)             = Note p  <$> changeDrn d
elementD (Rest d)               = Rest    <$> changeDrn d


changeDrn :: NoteLength -> DTMon Duration
changeDrn d                     = (durationT `flip` d) <$> get


durationT :: UnitNoteLength -> NoteLength -> Duration
durationT unl d = 
    let rat = rduration unl d in case rationalToDuration rat of
      Nothing -> dLonga
      Just ans -> ans
