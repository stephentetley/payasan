{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.InTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert LilyPond to Main Syntax.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.InTrans
  (
    translateFromInput
  , translateFromInput_DurationOnly
  ) where


import Payasan.Base.Internal.LilyPond.Syntax

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import Payasan.Base.Pitch



translateFromInput :: ScoreInfo 
                   -> Phrase LyPitch LyNoteLength anno 
                   -> Phrase Pitch Duration anno
translateFromInput info = pitchTrafo . transformD drn_algo
  where
    -- If AbsPitch then /previous pitch/ will never be used
    pitchTrafo = case global_ly_octave_mode info of
                    RelPitch pch -> transformP (rel_pch_algo pch)
                    AbsPitch -> transformP abs_pch_algo


translateFromInput_DurationOnly :: Phrase pch LyNoteLength anno 
                                -> Phrase pch Duration anno
translateFromInput_DurationOnly = transformD drn_algo

type DMon    a      = Mon Duration a
type RelPMon a      = Mon Pitch a
type AbsPMon a      = Mon () a



--------------------------------------------------------------------------------
-- Relative Pitch translation

rel_pch_algo :: Pitch -> BeamPitchAlgo Pitch LyPitch Pitch
rel_pch_algo start = BeamPitchAlgo
    { initial_stateP    = start
    , element_trafoP    = relElementP
    }


previousPitch :: RelPMon Pitch
previousPitch = get

setPrevPitch :: Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element LyPitch drn anno -> RelPMon (Element Pitch drn anno)
relElementP (NoteElem e a)      = (\e1 -> NoteElem e1 a) <$> relNoteP e
relElementP (Rest d)            = pure $ Rest d
relElementP (Chord ps d a)      = 
    (\ps1 -> Chord ps1 d a) <$> mapM changePitchRel ps

relElementP (Graces ns)         = Graces <$> mapM relNoteP ns
relElementP (Punctuation s)     = pure $ Punctuation s


relNoteP :: Note LyPitch drn -> RelPMon (Note Pitch drn)
relNoteP (Note pch drn)         = (\p -> Note p drn) <$> changePitchRel pch


changePitchRel :: LyPitch -> RelPMon Pitch
changePitchRel p1 = 
    do { p0 <- previousPitch
       ; let tp1 = toPitchRel p0 p1
       ; setPrevPitch tp1
       ; return tp1
       }
                              



--------------------------------------------------------------------------------
-- Abs Pitch translation


abs_pch_algo :: BeamPitchAlgo () LyPitch Pitch
abs_pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = absElementP
    }


absElementP :: Element LyPitch drn anno -> AbsPMon (Element Pitch drn anno)
absElementP (NoteElem e a)      = (\e1 -> NoteElem e1 a) <$> absNoteP e
absElementP (Rest d)            = pure $ Rest d
absElementP (Chord ps d a)      = 
    (\ps1 -> Chord ps1 d a) <$> mapM changePitchAbs ps

absElementP (Graces ns)         = Graces <$> mapM absNoteP ns
absElementP (Punctuation s)     = pure $ Punctuation s


absNoteP :: Note LyPitch drn -> AbsPMon (Note Pitch drn)
absNoteP (Note pch drn)         = (\p -> Note p drn) <$> changePitchAbs pch


changePitchAbs :: LyPitch -> AbsPMon Pitch
changePitchAbs p1 = return $ toPitchAbs p1




--------------------------------------------------------------------------------
-- Duration translation


drn_algo :: BeamDurationAlgo Duration LyNoteLength Duration 
drn_algo = BeamDurationAlgo
    { initial_stateD    = d_quarter
    , element_trafoD    = elementD
    }

previousDuration :: DMon Duration
previousDuration = get

setPrevDuration :: Duration -> DMon ()
setPrevDuration d = put d


elementD :: Element pch LyNoteLength anno -> DMon (Element pch Duration anno)
elementD (NoteElem e a)         = (\e1 -> NoteElem e1 a) <$> noteD e
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Chord ps d a)         = (\d1 -> Chord ps d1 a) <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns
elementD (Punctuation s)        = pure $ Punctuation s

noteD :: Note pch LyNoteLength -> DMon (Note pch Duration)
noteD (Note pch drn)            = Note pch <$> changeDrn drn



changeDrn :: LyNoteLength -> DMon Duration
changeDrn (DrnDefault)    = previousDuration
changeDrn (DrnExplicit d) = setPrevDuration d >> return d

