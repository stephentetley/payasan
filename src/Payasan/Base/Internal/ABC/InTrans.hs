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
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as PCH


translate :: Phrase Pitch NoteLength anno -> Phrase PCH.Pitch Duration anno
translate = P.transform pch_algo . D.transform drn_algo

type PTMon a = D.Mon () a
type DTMon a = D.Mon UnitNoteLength a

--------------------------------------------------------------------------------
-- Pitch translation


pch_algo :: P.BeamPitchAlgo () Pitch PCH.Pitch
pch_algo = P.BeamPitchAlgo
    { P.initial_state           = ()
    , P.element_trafo           = elementP
    }



elementP :: Element Pitch drn anno -> PTMon (Element PCH.Pitch drn anno)
elementP (NoteElem e a)         = (\e1 -> NoteElem e1 a)  <$> noteP e
elementP (Rest d)               = pure $ Rest d
elementP (Chord ps d a)         = 
    (\ps1 -> Chord ps1 d a) <$> mapM transPch ps

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
    , D.element_trafo           = elementD
    }

-- actionInfoD :: LocalRenderInfo -> DTMon ()
-- actionInfoD info = put (local_unit_note_len info)

elementD :: Element pch NoteLength anno -> DTMon (Element pch Duration anno)
elementD (NoteElem e a)         = (\e1 -> NoteElem e1 a) <$> noteD e
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Chord ps d a)         = (\d1 -> Chord ps  d1 a) <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns


noteD :: Note pch NoteLength -> DTMon (Note pch Duration)
noteD (Note pch drn)            = Note pch <$> changeDrn drn


changeDrn :: NoteLength -> DTMon Duration
changeDrn d                     = 
    (durationT `flip` d) <$> asksLocal local_unit_note_len


durationT :: UnitNoteLength -> NoteLength -> Duration
durationT unl d = 
    let rat = rduration unl d in case rationalToDuration rat of
      Nothing -> dLonga
      Just ans -> ans

