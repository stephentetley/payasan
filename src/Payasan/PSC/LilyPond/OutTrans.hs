{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.LilyPond.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015-2017
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

module Payasan.PSC.LilyPond.OutTrans
  (
    translateToLyPartOut_Relative
  , translateToLyPartOut_Absolute
  , translateToLyPartOut_DurationOnly
  ) where


import Payasan.PSC.Base.LilyPondCommon

import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals


import Payasan.Base.Duration
import Payasan.Base.Pitch



translateToLyPartOut_Relative :: Pitch
                              -> Part Pitch Duration anno 
                              -> LyPartOut anno
translateToLyPartOut_Relative pch = 
    transformP (rel_pch_algo pch) . transformD drn_algo


translateToLyPartOut_Absolute :: Part Pitch Duration anno 
                              -> LyPartOut anno
translateToLyPartOut_Absolute = 
    transformP abs_pch_algo . transformD drn_algo


translateToLyPartOut_DurationOnly :: Part pch Duration anno 
                                  -> GenLyPartOut pch anno
translateToLyPartOut_DurationOnly = transformD drn_algo

type DMon    a      = Mon Duration a
type RelPMon a      = Mon Pitch a
type AbsPMon a      = Mon () a


--------------------------------------------------------------------------------
-- Relative Pitch translation

rel_pch_algo :: Pitch -> ExtPitchAlgo Pitch Pitch LyPitch
rel_pch_algo start = ExtPitchAlgo
    { initial_stateP    = start
    , element_trafoP    = relElementP
    }


previousPitch :: RelPMon Pitch
previousPitch = get

setPrevPitch :: Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element Pitch drn anno -> RelPMon (Element LyPitch drn anno)
relElementP (Note p d a t)      = (\p1 -> Note p1 d a t) <$> changePitchRel p
relElementP (Rest d)            = pure $ Rest d
relElementP (Spacer d)          = pure $ Spacer d
relElementP (Skip d)            = pure $ Skip d
relElementP (Chord ps d a t)    = 
    (\ps1 -> Chord ps1 d a t) <$> mapM changePitchRel ps

relElementP (Graces ns)         = Graces <$> mapM relGrace1P ns
relElementP (Punctuation s)     = pure $ Punctuation s


relGrace1P :: Grace1 Pitch drn -> RelPMon (Grace1 LyPitch drn)
relGrace1P (Grace1 p d)         = (\p1 -> Grace1 p1 d) <$> changePitchRel p


changePitchRel :: Pitch -> RelPMon LyPitch
changePitchRel p1 = 
    do { p0 <- previousPitch
       ; let ply = fromPitchRel p0 p1
       ; setPrevPitch p1
       ; return ply
       }

--------------------------------------------------------------------------------
-- Abs Pitch translation


abs_pch_algo :: ExtPitchAlgo () Pitch LyPitch
abs_pch_algo = ExtPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = absElementP
    }


absElementP :: Element Pitch drn anno -> AbsPMon (Element LyPitch drn anno)
absElementP (Note p d a t)      = (\p1 -> Note p1 d a t) <$> changePitchAbs p
absElementP (Rest d)            = pure $ Rest d
absElementP (Spacer d)          = pure $ Spacer d
absElementP (Skip d)            = pure $ Skip d
absElementP (Chord ps d a t)    = 
    (\ps1 -> Chord ps1 d a t) <$> mapM changePitchAbs ps

absElementP (Graces ns)         = Graces <$> mapM absNoteP ns
absElementP (Punctuation s)     = pure $ Punctuation s


absNoteP :: Grace1 Pitch drn -> AbsPMon (Grace1 LyPitch drn)
absNoteP (Grace1 p d)           = (\p1 -> Grace1 p1 d) <$> changePitchAbs p


-- No previous pitch indicates Absolute pitch mode
changePitchAbs :: Pitch -> AbsPMon LyPitch
changePitchAbs p1 = return $ fromPitchAbs p1


--------------------------------------------------------------------------------
-- Duration translation

-- We always want the first duration to print in the output,
-- so the initial state is 0 which should never match.
--
drn_algo :: ExtDurationAlgo Duration Duration LyNoteLength
drn_algo = ExtDurationAlgo
    { initial_stateD    = d_zero        
    , element_trafoD    = elementD
    }

previousDuration :: DMon Duration
previousDuration = get

setPrevDuration :: Duration -> DMon ()
setPrevDuration d = put d

resetDuration :: DMon ()
resetDuration = put d_zero

-- | Spacer and Skip treated differently...
--
elementD :: Element pch Duration anno -> DMon (Element pch LyNoteLength anno)
elementD (Note p d a t)         = (\d1 -> Note p d1 a t) <$> changeDuration d
elementD (Rest d)               = Rest      <$> changeDuration d
elementD (Spacer d)             = Spacer    <$> changeDuration d
elementD (Skip d)               = Skip      <$> skipDuration d
elementD (Chord ps d a t)       = (\d1 -> Chord ps d1 a t) <$> changeDuration d
elementD (Graces ns)            = Graces    <$> mapM grace1D ns
elementD (Punctuation s)        = pure $ Punctuation s

grace1D :: Grace1 pch Duration -> DMon (Grace1 pch LyNoteLength)
grace1D (Grace1 p d)            = Grace1 p <$> changeDuration d



changeDuration :: Duration -> DMon LyNoteLength
changeDuration d1 =
   do { d0 <- previousDuration 
      ; if d1 == d0 
          then return DrnDefault
          else setPrevDuration d1 >> return (DrnExplicit d1)
      }


-- | skip always prints an explicit duration, it also resets the 
-- duration so the next note is explicit too.
--
skipDuration :: Duration -> DMon LyNoteLength
skipDuration d1 = resetDuration >> return (DrnExplicit d1)
