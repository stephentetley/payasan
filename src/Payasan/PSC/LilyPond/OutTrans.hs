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


import Payasan.PSC.LilyPond.Common

import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals


import Payasan.Base.Duration
import Payasan.Base.Pitch



translateToLyPartOut_Relative :: Pitch
                              -> Part Pitch Duration anno 
                              -> Part LyPitch LyNoteLength anno
translateToLyPartOut_Relative pch = 
    transformExternal (rel_pch_algo pch) . transformExternal drn_algo


translateToLyPartOut_Absolute :: Part Pitch Duration anno 
                              -> Part LyPitch LyNoteLength anno
translateToLyPartOut_Absolute = 
    absPitchTrafo . transformExternal drn_algo


translateToLyPartOut_DurationOnly :: Part pch Duration anno 
                                  -> Part pch LyNoteLength anno
translateToLyPartOut_DurationOnly = transformExternal drn_algo


--------------------------------------------------------------------------------
-- Relative Pitch translation

rel_pch_algo :: Pitch -> ExternalAlgo Pitch Pitch LyPitch drn drn anno anno
rel_pch_algo start = ExternalAlgo
    { initial_state     = start
    , element_trafo     = relElementP
    }

type RelPMon a      = Mon Pitch a


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


absPitchTrafo :: Part Pitch drn anno -> Part LyPitch drn anno
absPitchTrafo = mapPitch fromPitchAbs


--------------------------------------------------------------------------------
-- Duration translation

-- We always want the first duration to print in the output,
-- so the initial state is 0 which should never match.
--
drn_algo :: ExternalAlgo Duration pch pch Duration LyNoteLength anno anno
drn_algo = ExternalAlgo
    { initial_state     = d_zero        
    , element_trafo     = elementD
    }


type DMon    a      = Mon Duration a


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
