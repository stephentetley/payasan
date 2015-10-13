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



import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Monophonic.Internal.Traversals

import Payasan.Base.Internal.LilyPond.Syntax

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import Payasan.Base.Pitch



lilyPondTranslate :: ScoreInfo
                  -> GenLyMonoPhrase LyPitch anno 
                  -> Phrase Pitch Duration anno
lilyPondTranslate info = pitchTrafo . trafoDuration
  where
    -- If AbsPitch then /previous pitch/ will never be used
    pitchTrafo = case global_ly_octave_mode info of
                    RelPitch pch -> trafoRelPitch pch
                    AbsPitch -> trafoAbsPitch


type DMon a    = Mon Duration a
type RelPMon a = Mon Pitch a
type AbsPMon a = Mon () a



--------------------------------------------------------------------------------
-- Relative Pitch translation

trafoRelPitch :: Pitch -> Phrase LyPitch drn anno -> Phrase Pitch drn anno
trafoRelPitch p0 = transformP (rel_pch_algo p0)

rel_pch_algo :: Pitch -> MonoPitchAlgo Pitch LyPitch Pitch
rel_pch_algo start = MonoPitchAlgo
    { initial_stateP    = start
    , element_trafoP    = relElementP
    }


previousPitch :: RelPMon Pitch
previousPitch = get

setPrevPitch :: Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element LyPitch drn anno -> RelPMon (Element Pitch drn anno)
relElementP (Note p d a)        = (\p1 -> Note p1 d a) <$> changePitchRel p
relElementP (Rest d)            = pure $ Rest d
relElementP (Punctuation s)     = pure $ Punctuation s



changePitchRel :: LyPitch -> RelPMon Pitch
changePitchRel p1 = 
    do { p0 <- previousPitch
       ; let tp1 = toPitchRel p0 p1
       ; setPrevPitch tp1
       ; return tp1
       }
                              



--------------------------------------------------------------------------------
-- Abs Pitch translation

trafoAbsPitch :: Phrase LyPitch drn anno -> Phrase Pitch drn anno
trafoAbsPitch = transformP abs_pch_algo


abs_pch_algo :: MonoPitchAlgo () LyPitch Pitch
abs_pch_algo = MonoPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = absElementP
    }


absElementP :: Element LyPitch drn anno -> AbsPMon (Element Pitch drn anno)
absElementP (Note p d a)        = (\p1 -> Note p1 d a) <$> changePitchAbs p
absElementP (Rest d)            = pure $ Rest d
absElementP (Punctuation s)     = pure $ Punctuation s


changePitchAbs :: LyPitch -> AbsPMon Pitch
changePitchAbs p1 = return $ toPitchAbs p1




--------------------------------------------------------------------------------
-- Duration translation

trafoDuration :: Phrase pch LyNoteLength anno -> Phrase pch Duration anno
trafoDuration = transformD drn_algo


drn_algo :: MonoDurationAlgo Duration LyNoteLength Duration 
drn_algo = MonoDurationAlgo
    { initial_stateD    = d_quarter
    , element_trafoD    = elementD
    }

previousDuration :: DMon Duration
previousDuration = get

setPrevDuration :: Duration -> DMon ()
setPrevDuration d = put d


elementD :: Element pch LyNoteLength anno -> DMon (Element pch Duration anno)
elementD (Note p d a)           = (\d1 -> Note p d1 a) <$> changeDrn d
elementD (Rest d)               = Rest   <$> changeDrn d
elementD (Punctuation s)        = pure $ Punctuation s


changeDrn :: LyNoteLength -> DMon Duration
changeDrn (DrnDefault)          = previousDuration
changeDrn (DrnExplicit d)       = setPrevDuration d >> return d

