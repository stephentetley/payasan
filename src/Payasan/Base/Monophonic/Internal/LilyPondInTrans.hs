{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.LilyPondInTrans
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

module Payasan.Base.Monophonic.Internal.LilyPondInTrans
  (
    lilyPondTranslate
  ) where



import Payasan.Base.Monophonic.Internal.MonoDurationTrafo as D
import Payasan.Base.Monophonic.Internal.MonoPitchTrafo as P
import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.LilyPond.Syntax (Pitch,NoteLength(..))
import Payasan.Base.Internal.LilyPond.Utils
import Payasan.Base.Internal.Utils

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as PCH



lilyPondTranslate :: GlobalRenderInfo 
                  -> Phrase Pitch NoteLength -> Phrase PCH.Pitch Duration 
lilyPondTranslate info = pitchTrafo . D.transform drn_algo
  where
    -- If AbsPitch then /previous pitch/ will never be used
    pitchTrafo = case global_pitch_directive info of
                    RelPitch pch -> P.transform (rel_pch_algo pch)
                    AbsPitch -> P.transform abs_pch_algo


type DTMon a = D.Mon Duration a
type RelPMon a = D.Mon PCH.Pitch a
type AbsPMon a = D.Mon () a



--------------------------------------------------------------------------------
-- Relative Pitch translation

rel_pch_algo :: PCH.Pitch -> P.MonoPitchAlgo PCH.Pitch Pitch PCH.Pitch
rel_pch_algo start = P.MonoPitchAlgo
    { P.initial_state           = start
    , P.bar_info_action         = \_ -> return ()
    , P.element_trafo           = relElementP
    }


previousPitch :: RelPMon PCH.Pitch
previousPitch = get

setPrevPitch :: PCH.Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element Pitch drn -> RelPMon (Element PCH.Pitch drn)
relElementP (Note p d)          = (\pch -> Note pch d) <$> changePitchRel p
relElementP (Rest d)            = pure $ Rest d



changePitchRel :: Pitch -> RelPMon PCH.Pitch
changePitchRel p1 = 
    do { p0 <- previousPitch
       ; let tp1 = toPitchRel p1 p0
       ; setPrevPitch tp1
       ; return tp1
       }
                              



--------------------------------------------------------------------------------
-- Abs Pitch translation


abs_pch_algo :: P.MonoPitchAlgo () Pitch PCH.Pitch
abs_pch_algo = P.MonoPitchAlgo
    { P.initial_state           = ()
    , P.bar_info_action         = \_ -> return ()
    , P.element_trafo           = absElementP
    }


absElementP :: Element Pitch drn -> AbsPMon (Element PCH.Pitch drn)
absElementP (Note p d)          = (\pch -> Note pch d) <$> changePitchAbs p
absElementP (Rest d)            = pure $ Rest d


changePitchAbs :: Pitch -> AbsPMon PCH.Pitch
changePitchAbs p1 = return $ toPitchAbs p1




--------------------------------------------------------------------------------
-- Duration translation


drn_algo :: D.MonoDurationAlgo Duration NoteLength Duration 
drn_algo = D.MonoDurationAlgo
    { D.initial_state           = dQuarter
    , D.bar_info_action         = \_ -> return ()
    , D.element_trafo           = elementD
    }

previousDuration :: DTMon Duration
previousDuration = get

setPrevDuration :: Duration -> DTMon ()
setPrevDuration d = put d


elementD :: Element pch NoteLength -> DTMon (Element pch Duration)
elementD (Note pch drn)         = Note pch <$> changeDrn drn
elementD (Rest d)               = Rest      <$> changeDrn d


changeDrn :: NoteLength -> DTMon Duration
changeDrn (DrnDefault)    = previousDuration
changeDrn (DrnExplicit d) = setPrevDuration d >> return d


{-

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

-}