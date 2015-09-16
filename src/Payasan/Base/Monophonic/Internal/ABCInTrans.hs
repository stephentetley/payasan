{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.ABCInTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert ABC to Monophonic syntax.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.ABCInTrans
  (
    abcTranslate
  ) where



import Payasan.Base.Monophonic.Internal.MonoDurationTrafo as D
import Payasan.Base.Monophonic.Internal.MonoPitchTrafo as P
import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.ABC.Syntax (Pitch,NoteLength)
import Payasan.Base.Internal.ABC.Utils
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as PCH


abcTranslate :: ABCMonoPhrase -> Phrase PCH.Pitch Duration ()
abcTranslate = P.transform pch_algo . D.transform drn_algo

type PTMon a = D.Mon () a
type DTMon a = D.Mon UnitNoteLength a

--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: P.MonoPitchAlgo () Pitch PCH.Pitch
pch_algo = P.MonoPitchAlgo
    { P.initial_state           = ()
    , P.element_trafo           = elementP
    }


elementP :: Element Pitch drn anno -> PTMon (Element PCH.Pitch drn anno) 
elementP (Note p d a)           = (\p1 -> Note p1 d a) <$> transPch p
elementP (Rest d)               = pure $ Rest d


-- likely to change wrt key sig...
transPch :: Pitch -> PTMon PCH.Pitch
transPch = pure . toPitch



--------------------------------------------------------------------------------
-- Translate duration

drn_algo :: D.MonoDurationAlgo UnitNoteLength NoteLength Duration
drn_algo = D.MonoDurationAlgo
    { D.initial_state           = UNIT_NOTE_8
    , D.element_trafo           = elementD
    }


elementD :: Element pch NoteLength anno -> DTMon (Element pch Duration anno)
elementD (Note p d a)           = (\d1 -> Note p d1 a) <$> changeDrn d
elementD (Rest d)               = Rest    <$> changeDrn d


changeDrn :: NoteLength -> DTMon Duration
changeDrn d = (durationT `flip` d) <$> asksLocal local_unit_note_len


durationT :: UnitNoteLength -> NoteLength -> Duration
durationT unl d = 
    let rat = rduration unl d in case rationalToDuration rat of
      Nothing -> dLonga
      Just ans -> ans

