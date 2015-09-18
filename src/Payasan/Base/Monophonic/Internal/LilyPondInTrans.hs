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

  , trafoRelPitch
  , trafoAbsPitch
  , trafoDuration

  ) where



import Payasan.Base.Monophonic.Internal.MonoDurationTrafo as D
import Payasan.Base.Monophonic.Internal.MonoPitchTrafo as P
import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.LilyPond.Syntax (Pitch,NoteLength(..))
import Payasan.Base.Internal.LilyPond.Utils
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as BASE



lilyPondTranslate :: GlobalRenderInfo 
                  -> GenLyMonoPhrase Pitch anno 
                  -> Phrase BASE.Pitch Duration anno
lilyPondTranslate info = pitchTrafo . trafoDuration
  where
    -- If AbsPitch then /previous pitch/ will never be used
    pitchTrafo = case global_ly_octave_mode info of
                    RelPitch pch -> trafoRelPitch pch
                    AbsPitch -> trafoAbsPitch


type DTMon a = D.Mon Duration a
type RelPMon a = D.Mon BASE.Pitch a
type AbsPMon a = D.Mon () a



--------------------------------------------------------------------------------
-- Relative Pitch translation

trafoRelPitch :: BASE.Pitch -> Phrase Pitch drn anno -> Phrase BASE.Pitch drn anno
trafoRelPitch p0 = P.transform (rel_pch_algo p0)

rel_pch_algo :: BASE.Pitch -> P.MonoPitchAlgo BASE.Pitch Pitch BASE.Pitch
rel_pch_algo start = P.MonoPitchAlgo
    { P.initial_state           = start
    , P.element_trafo           = relElementP
    }


previousPitch :: RelPMon BASE.Pitch
previousPitch = get

setPrevPitch :: BASE.Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element Pitch drn anno -> RelPMon (Element BASE.Pitch drn anno)
relElementP (Note p d a)        = (\p1 -> Note p1 d a) <$> changePitchRel p
relElementP (Rest d)            = pure $ Rest d



changePitchRel :: Pitch -> RelPMon BASE.Pitch
changePitchRel p1 = 
    do { p0 <- previousPitch
       ; let tp1 = toPitchRel p1 p0
       ; setPrevPitch tp1
       ; return tp1
       }
                              



--------------------------------------------------------------------------------
-- Abs Pitch translation

trafoAbsPitch :: Phrase Pitch drn anno -> Phrase BASE.Pitch drn anno
trafoAbsPitch = P.transform abs_pch_algo


abs_pch_algo :: P.MonoPitchAlgo () Pitch BASE.Pitch
abs_pch_algo = P.MonoPitchAlgo
    { P.initial_state           = ()
    , P.element_trafo           = absElementP
    }


absElementP :: Element Pitch drn anno -> AbsPMon (Element BASE.Pitch drn anno)
absElementP (Note p d a)        = (\p1 -> Note p1 d a) <$> changePitchAbs p
absElementP (Rest d)            = pure $ Rest d


changePitchAbs :: Pitch -> AbsPMon BASE.Pitch
changePitchAbs p1 = return $ toPitchAbs p1




--------------------------------------------------------------------------------
-- Duration translation

trafoDuration :: Phrase pch NoteLength anno -> Phrase pch Duration anno
trafoDuration = D.transform drn_algo


drn_algo :: D.MonoDurationAlgo Duration NoteLength Duration 
drn_algo = D.MonoDurationAlgo
    { D.initial_state           = dQuarter
    , D.element_trafo           = elementD
    }

previousDuration :: DTMon Duration
previousDuration = get

setPrevDuration :: Duration -> DTMon ()
setPrevDuration d = put d


elementD :: Element pch NoteLength anno -> DTMon (Element pch Duration anno)
elementD (Note p d a)           = (\d1 -> Note p d1 a) <$> changeDrn d
elementD (Rest d)               = Rest   <$> changeDrn d


changeDrn :: NoteLength -> DTMon Duration
changeDrn (DrnDefault)    = previousDuration
changeDrn (DrnExplicit d) = setPrevDuration d >> return d

