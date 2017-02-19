{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.InTrans
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert LilyPond to (standard) Cadenza syntax.
--
--------------------------------------------------------------------------------

module Payasan.Score.Cadenza.Internal.InTrans
  (

    lilyPondTranslate_Relative
  , lilyPondTranslate_Absolute

  , trafoRelPitch
  , trafoAbsPitch
  , trafoDuration

  ) where



import Payasan.Score.Cadenza.Internal.Syntax
import Payasan.Score.Cadenza.Internal.Traversals

import Payasan.PSC.LilyPond.Common

import Payasan.Base.Duration
import Payasan.Base.Pitch

import Control.Monad.State              -- package: mtl

lilyPondTranslate_Relative :: Pitch
                           -> LyCadenzaSection2 LyPitch anno 
                           -> StdCadenzaSection1 anno
lilyPondTranslate_Relative pch = trafoRelPitch pch . trafoDuration


lilyPondTranslate_Absolute :: LyCadenzaSection2 LyPitch anno 
                           -> StdCadenzaSection1 anno
lilyPondTranslate_Absolute = trafoAbsPitch . trafoDuration


type DMon a    = Mon Duration a
type RelPMon a = Mon Pitch a
type AbsPMon a = Mon () a



--------------------------------------------------------------------------------
-- Relative Pitch translation

trafoRelPitch :: Pitch -> Section LyPitch drn anno -> Section Pitch drn anno
trafoRelPitch p0 = transformCadenza (rel_pch_algo p0)

rel_pch_algo :: Pitch -> CadenzaAlgo Pitch LyPitch Pitch drn drn anno anno
rel_pch_algo start = CadenzaAlgo
    { initial_state = start
    , element_trafo = relElementP
    }


previousPitch :: RelPMon Pitch
previousPitch = get

setPrevPitch :: Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element LyPitch drn anno -> RelPMon (Element Pitch drn anno)
relElementP (Note p d a t)      = (\p1 -> Note p1 d a t) <$> changePitchRel p
relElementP (Rest d)            = pure $ Rest d
relElementP (Spacer d)          = pure $ Spacer d
relElementP (Skip d)            = pure $ Skip d
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

trafoAbsPitch :: Section LyPitch drn anno -> Section Pitch drn anno
trafoAbsPitch = transformCadenza abs_pch_algo


abs_pch_algo :: CadenzaAlgo () LyPitch Pitch drn drn anno anno
abs_pch_algo = CadenzaAlgo
    { initial_state = ()
    , element_trafo = absElementP
    }


absElementP :: Element LyPitch drn anno -> AbsPMon (Element Pitch drn anno)
absElementP (Note p d a t)      = (\p1 -> Note p1 d a t) <$> changePitchAbs p
absElementP (Rest d)            = pure $ Rest d
absElementP (Spacer d)          = pure $ Spacer d
absElementP (Skip d)            = pure $ Skip d
absElementP (Punctuation s)     = pure $ Punctuation s


changePitchAbs :: LyPitch -> AbsPMon Pitch
changePitchAbs p1 = return $ toPitchAbs p1




--------------------------------------------------------------------------------
-- Duration translation

trafoDuration :: Section pch LyNoteLength anno -> Section pch Duration anno
trafoDuration = transformCadenza drn_algo


drn_algo :: CadenzaAlgo Duration pch pch LyNoteLength Duration anno anno
drn_algo = CadenzaAlgo
    { initial_state = d_quarter
    , element_trafo = elementD
    }

previousDuration :: DMon Duration
previousDuration = get

setPrevDuration :: Duration -> DMon ()
setPrevDuration d = put d


elementD :: Element pch LyNoteLength anno -> DMon (Element pch Duration anno)
elementD (Note p d a t)         = (\d1 -> Note p d1 a t) <$> changeDuration d
elementD (Rest d)               = Rest   <$> changeDuration d
elementD (Spacer d)             = Spacer <$> changeDuration d
elementD (Skip d)               = Skip   <$> skipDuration d
elementD (Punctuation s)        = pure $ Punctuation s


changeDuration :: LyNoteLength -> DMon Duration
changeDuration (DrnDefault)         = previousDuration
changeDuration (DrnExplicit d)      = setPrevDuration d >> return d

skipDuration :: LyNoteLength -> DMon Duration
skipDuration (DrnDefault)           = previousDuration
skipDuration (DrnExplicit d)        = return d

