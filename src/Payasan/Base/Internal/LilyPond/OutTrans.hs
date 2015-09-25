{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert pitch and duration to their LilyPond equivalents 
-- prior to printing.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.OutTrans
  (
    translate
  , translateDurationOnly
  ) where



import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import Payasan.Base.Pitch



translate :: GlobalRenderInfo 
          -> Phrase Pitch Duration anno 
          -> Phrase LyPitch LyNoteLength anno
translate info = pitchTrafo . transformD drn_algo
  where
    -- If AbsPitch then /previous pitch/ will never be used
    pitchTrafo = case global_ly_octave_mode info of
                    RelPitch pch -> transformP (rel_pch_algo pch)
                    AbsPitch -> transformP abs_pch_algo



translateDurationOnly :: Phrase pch Duration anno 
                      -> Phrase pch LyNoteLength anno
translateDurationOnly = transformD drn_algo

type DMon    a      = Mon Duration a
type RelPMon a      = Mon Pitch a
type AbsPMon a      = Mon () a


--------------------------------------------------------------------------------
-- Relative Pitch translation

rel_pch_algo :: Pitch -> BeamPitchAlgo Pitch Pitch LyPitch
rel_pch_algo start = BeamPitchAlgo
    { initial_stateP    = start
    , element_trafoP    = relElementP
    }


previousPitch :: RelPMon Pitch
previousPitch = get

setPrevPitch :: Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element Pitch drn anno -> RelPMon (Element LyPitch drn anno)
relElementP (NoteElem e a)      = (\e1 -> NoteElem e1 a) <$> relNoteP e
relElementP (Rest d)            = pure $ Rest d
relElementP (Chord ps d a)      = 
    (\ps1 -> Chord ps1 d a) <$> mapM changePitchRel ps

relElementP (Graces ns)         = Graces <$> mapM relNoteP ns


relNoteP :: Note Pitch drn -> RelPMon (Note LyPitch drn)
relNoteP (Note pch drn)         = (\p -> Note p drn) <$> changePitchRel pch


changePitchRel :: Pitch -> RelPMon LyPitch
changePitchRel p1 = 
    do { p0 <- previousPitch
       ; let ply = fromPitchRel p0 p1
       ; setPrevPitch p1
       ; return ply
       }

--------------------------------------------------------------------------------
-- Abs Pitch translation


abs_pch_algo :: BeamPitchAlgo () Pitch LyPitch
abs_pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = absElementP
    }


absElementP :: Element Pitch drn anno -> AbsPMon (Element LyPitch drn anno)
absElementP (NoteElem e a)      = (\e1 -> NoteElem e1 a) <$> absNoteP e
absElementP (Rest d)            = pure $ Rest d
absElementP (Chord ps d a)      = 
    (\ps1 -> Chord ps1 d a)  <$> mapM changePitchAbs ps

absElementP (Graces ns)         = Graces <$> mapM absNoteP ns


absNoteP :: Note Pitch drn -> AbsPMon (Note LyPitch drn)
absNoteP (Note pch drn)         = (\p -> Note p drn) <$> changePitchAbs pch


-- No previous pitch indicates Absolute pitch mode
changePitchAbs :: Pitch -> AbsPMon LyPitch
changePitchAbs p1 = return $ fromPitchAbs p1


--------------------------------------------------------------------------------
-- Duration translation


drn_algo :: BeamDurationAlgo Duration Duration LyNoteLength
drn_algo = BeamDurationAlgo
    { initial_stateD    = d_quarter
    , element_trafoD    = elementD
    }

previousDuration :: DMon Duration
previousDuration = get

setPrevDuration :: Duration -> DMon ()
setPrevDuration d = put d


elementD :: Element pch Duration anno -> DMon (Element pch LyNoteLength anno)
elementD (NoteElem e a)         = (\e1 -> NoteElem  e1 a) <$> noteD e
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Chord ps d a)         = (\d1 -> Chord ps d1 a) <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns

noteD :: Note pch Duration -> DMon (Note pch LyNoteLength)
noteD (Note pch drn)            = Note pch <$> changeDrn drn



changeDrn :: Duration -> DMon LyNoteLength
changeDrn d1 =
   do { d0 <- previousDuration 
      ; if d1 == d0 
          then return DrnDefault
          else setPrevDuration d1 >> return (DrnExplicit d1)
      }


