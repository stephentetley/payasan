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
    translateToOutput_Relative
  , translateToOutput_Absolute
  , translateToOutput_DurationOnly
  ) where



import Payasan.Base.Internal.LilyPond.Syntax

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import Payasan.Base.Pitch



translateToOutput_Relative :: Pitch
                           -> Phrase Pitch Duration anno 
                           -> Phrase LyPitch LyNoteLength anno
translateToOutput_Relative pch = 
    transformP (rel_pch_algo pch) . transformD drn_algo


translateToOutput_Absolute :: Phrase Pitch Duration anno 
                           -> Phrase LyPitch LyNoteLength anno
translateToOutput_Absolute = 
    transformP abs_pch_algo . transformD drn_algo


translateToOutput_DurationOnly :: Phrase pch Duration anno 
                               -> Phrase pch LyNoteLength anno
translateToOutput_DurationOnly = transformD drn_algo

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
relElementP (NoteElem e a t m)  = (\e1 -> NoteElem e1 a t m) <$> relNoteP e
relElementP (Rest d)            = pure $ Rest d
relElementP (Skip d)            = pure $ Skip d
relElementP (Chord ps d a t m)  = 
    (\ps1 -> Chord ps1 d a t m) <$> mapM changePitchRel ps

relElementP (Graces ns)         = Graces <$> mapM relNoteP ns
relElementP (Punctuation s)     = pure $ Punctuation s


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
absElementP (NoteElem e a t m)  = (\e1 -> NoteElem e1 a t m) <$> absNoteP e
absElementP (Rest d)            = pure $ Rest d
absElementP (Skip d)            = pure $ Skip d
absElementP (Chord ps d a t m)  = 
    (\ps1 -> Chord ps1 d a t m) <$> mapM changePitchAbs ps

absElementP (Graces ns)         = Graces <$> mapM absNoteP ns
absElementP (Punctuation s)     = pure $ Punctuation s


absNoteP :: Note Pitch drn -> AbsPMon (Note LyPitch drn)
absNoteP (Note pch drn)         = (\p -> Note p drn) <$> changePitchAbs pch


-- No previous pitch indicates Absolute pitch mode
changePitchAbs :: Pitch -> AbsPMon LyPitch
changePitchAbs p1 = return $ fromPitchAbs p1


--------------------------------------------------------------------------------
-- Duration translation

-- We always want the first duration to print in the output,
-- so the initial state is 0 which should never match.
--
drn_algo :: BeamDurationAlgo Duration Duration LyNoteLength
drn_algo = BeamDurationAlgo
    { initial_stateD    = d_zero        
    , element_trafoD    = elementD
    }

previousDuration :: DMon Duration
previousDuration = get

setPrevDuration :: Duration -> DMon ()
setPrevDuration d = put d

resetDuration :: DMon ()
resetDuration = put d_zero


elementD :: Element pch Duration anno -> DMon (Element pch LyNoteLength anno)
elementD (NoteElem e a t m)     = (\e1 -> NoteElem e1 a t m) <$> noteD e
elementD (Rest d)               = Rest      <$> changeDuration d
elementD (Skip d)               = Skip      <$> skipDuration d
elementD (Chord ps d a t m)     = (\d1 -> Chord ps d1 a t m) <$> changeDuration d
elementD (Graces ns)            = Graces    <$> mapM noteD ns
elementD (Punctuation s)        = pure $ Punctuation s

noteD :: Note pch Duration -> DMon (Note pch LyNoteLength)
noteD (Note pch drn)            = Note pch <$> changeDuration drn



changeDuration :: Duration -> DMon LyNoteLength
changeDuration d1 =
   do { d0 <- previousDuration 
      ; if d1 == d0 
          then return DrnDefault
          else setPrevDuration d1 >> return (DrnExplicit d1)
      }


skipDuration :: Duration -> DMon LyNoteLength
skipDuration d1 =
   do { d0 <- previousDuration 
      ; if d1 == d0 
          then return DrnDefault
          else resetDuration >> return (DrnExplicit d1)
      }