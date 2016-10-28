{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.InTrans
-- Copyright   :  (c) Stephen Tetley 2015-2016
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
    translateFromInput_Relative
  , translateFromInput_Absolute
  , translateFromInput_DurationOnly
  ) where


import Payasan.Base.Internal.LilyPond.Syntax

import Payasan.Repr.IRBeam.Syntax
import Payasan.Repr.IRBeam.Traversals
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import Payasan.Base.Pitch



translateFromInput_Relative :: Pitch
                            -> Part LyPitch LyNoteLength anno 
                            -> Part Pitch Duration anno
translateFromInput_Relative pch  = 
    transformP (rel_pch_algo pch) . transformD drn_algo


translateFromInput_Absolute :: Part LyPitch LyNoteLength anno 
                            -> Part Pitch Duration anno
translateFromInput_Absolute = 
    transformP abs_pch_algo . transformD drn_algo


translateFromInput_DurationOnly :: Part pch LyNoteLength anno 
                                -> Part pch Duration anno
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
relElementP (NoteElem e a t)    = (\e1 -> NoteElem e1 a t) <$> relNoteP e
relElementP (Rest d)            = pure $ Rest d
relElementP (Spacer d)          = pure $ Spacer d
relElementP (Skip d)            = pure $ Skip d
relElementP (Chord ps d a t)    = 
    (\ps1 -> Chord ps1 d a t) <$> mapM changePitchRel ps

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
absElementP (NoteElem e a t)  = (\e1 -> NoteElem e1 a t) <$> absNoteP e
absElementP (Rest d)            = pure $ Rest d
absElementP (Spacer d)          = pure $ Spacer d
absElementP (Skip d)            = pure $ Skip d
absElementP (Chord ps d a t)    = 
    (\ps1 -> Chord ps1 d a t) <$> mapM changePitchAbs ps

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

-- | Spacer and Skip treated differently...
--
elementD :: Element pch LyNoteLength anno -> DMon (Element pch Duration anno)
elementD (NoteElem e a t)       = (\e1 -> NoteElem e1 a t) <$> noteD e
elementD (Rest d)               = Rest      <$> changeDuration d
elementD (Spacer d)             = Spacer    <$> changeDuration d
elementD (Skip d)               = Rest      <$> skipDuration d
elementD (Chord ps d a t)       = (\d1 -> Chord ps d1 a t) <$> changeDuration d
elementD (Graces ns)            = Graces    <$> mapM noteD ns
elementD (Punctuation s)        = pure $ Punctuation s

noteD :: Note pch LyNoteLength -> DMon (Note pch Duration)
noteD (Note pch drn)            = Note pch <$> changeDuration drn



changeDuration :: LyNoteLength -> DMon Duration
changeDuration (DrnDefault)    = previousDuration
changeDuration (DrnExplicit d) = setPrevDuration d >> return d

skipDuration :: LyNoteLength -> DMon Duration
skipDuration (DrnDefault)    = previousDuration
skipDuration (DrnExplicit d) = return d

