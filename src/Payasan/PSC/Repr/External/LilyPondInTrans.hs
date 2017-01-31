{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.External.LilyPondInTrans
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert LilyPond to Main Syntax.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.External.LilyPondInTrans
  (
    unquoteLyRelative
  , unquoteLyAbsolute
  , unquoteGenLy
  
  -- * DEPRECATED
--  , translateFromInput_Relative
--  , translateFromInput_Absolute
--  , translateFromInput_DurationOnly
  ) where


import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.PSC.Base.LilyPondCommon
import Payasan.PSC.Base.SyntaxCommon


import Payasan.Base.Duration
import Payasan.Base.Pitch

    
type DMon    a      = Mon Duration a
type RelPMon a      = Mon Pitch a
type AbsPMon a      = Mon () a

unquoteLyRelative :: String -> SectionInfo -> Pitch -> LySectionQuote anno -> Section Pitch Duration anno
unquoteLyRelative name info rpitch (LySectionQuote bs) =
    let bars = translateDuration info $ translatePitchRelative info rpitch bs
    in Section { section_name      = name
               , section_info      = info
               , section_bars      = bars
               }
               
unquoteLyAbsolute :: String -> SectionInfo -> LySectionQuote anno -> Section Pitch Duration anno
unquoteLyAbsolute name info (LySectionQuote bs) =
    let bars = translateDuration info $ translatePitchAbsolute info bs
    in Section { section_name      = name
               , section_info      = info
               , section_bars      = bars
               }

unquoteGenLy :: String -> SectionInfo -> GenLySectionQuote pch anno -> Section pch Duration anno
unquoteGenLy name info (GenLySectionQuote bs) =
    let bars = translateDuration info bs
    in Section { section_name      = name
               , section_info      = info
               , section_bars      = bars
               }

               
{-
               
-- | DEPRECATED - input should be translated from LySectionQuote
translateFromInput_Relative :: Pitch
                            -> Part LyPitch LyNoteLength anno 
                            -> Part Pitch Duration anno
translateFromInput_Relative pch  = 
    transformP (rel_pch_algo pch) . transformD drn_algo


-- | DEPRECATED - input should be translated from LySectionQuote
translateFromInput_Absolute :: Part LyPitch LyNoteLength anno 
                            -> Part Pitch Duration anno
translateFromInput_Absolute = 
    transformP abs_pch_algo . transformD drn_algo

-- | DEPRECATED - input should be translated from GenLySectionQuote
translateFromInput_DurationOnly :: Part pch LyNoteLength anno 
                                -> Part pch Duration anno
translateFromInput_DurationOnly = transformD drn_algo



-- | DEPRECATED
rel_pch_algo :: Pitch -> ExtPitchAlgo Pitch LyPitch Pitch
rel_pch_algo start = ExtPitchAlgo
    { initial_stateP    = start
    , element_trafoP    = relElementP
    }

-- | DEPRECATED
abs_pch_algo :: ExtPitchAlgo () LyPitch Pitch
abs_pch_algo = ExtPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = absElementP
    }

-- | DEPRECATED
drn_algo :: ExtDurationAlgo Duration LyNoteLength Duration 
drn_algo = ExtDurationAlgo
    { initial_stateD    = d_quarter
    , element_trafoD    = elementD
    }
   


-}

--------------------------------------------------------------------------------
-- Relative Pitch translation

    
translatePitchRelative :: SectionInfo 
                       -> Pitch 
                       -> [Bar LyPitch drn anno] 
                       -> [Bar Pitch drn anno]
translatePitchRelative info rpitch = 
    genTransformBars relElementP info rpitch 

previousPitch :: RelPMon Pitch
previousPitch = get

setPrevPitch :: Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element LyPitch drn anno -> RelPMon (Element Pitch drn anno)
relElementP (Note p d a t)      = (\p1 -> Note p1 d a t) <$> changePitchRel p
relElementP (Rest d)            = pure $ Rest d
relElementP (Spacer d)          = pure $ Spacer d
relElementP (Skip d)            = pure $ Skip d
relElementP (Chord ps d a t)    = 
    (\ps1 -> Chord ps1 d a t) <$> mapM changePitchRel ps

relElementP (Graces ns)         = Graces <$> mapM relGrace1P ns
relElementP (Punctuation s)     = pure $ Punctuation s


relGrace1P :: Grace1 LyPitch drn -> RelPMon (Grace1 Pitch drn)
relGrace1P (Grace1 p d)         = (\p1 -> Grace1 p1 d) <$> changePitchRel p


changePitchRel :: LyPitch -> RelPMon Pitch
changePitchRel p1 = 
    do { p0 <- previousPitch
       ; let tp1 = toPitchRel p0 p1
       ; setPrevPitch tp1
       ; return tp1
       }
                              



--------------------------------------------------------------------------------
-- Abs Pitch translation


translatePitchAbsolute :: SectionInfo 
                       -> [Bar LyPitch drn anno] 
                       -> [Bar Pitch drn anno]
translatePitchAbsolute info = 
    genTransformBars absElementP info () 


absElementP :: Element LyPitch drn anno -> AbsPMon (Element Pitch drn anno)
absElementP (Note p d a t)      = (\p1 -> Note p1 d a t) <$> changePitchAbs p
absElementP (Rest d)            = pure $ Rest d
absElementP (Spacer d)          = pure $ Spacer d
absElementP (Skip d)            = pure $ Skip d
absElementP (Chord ps d a t)    = 
    (\ps1 -> Chord ps1 d a t) <$> mapM changePitchAbs ps

absElementP (Graces ns)         = Graces <$> mapM absGrace1P ns
absElementP (Punctuation s)     = pure $ Punctuation s


absGrace1P :: Grace1 LyPitch drn -> AbsPMon (Grace1 Pitch drn)
absGrace1P (Grace1 p d)         = (\p1 -> Grace1 p1 d) <$> changePitchAbs p


changePitchAbs :: LyPitch -> AbsPMon Pitch
changePitchAbs p1 = return $ toPitchAbs p1




--------------------------------------------------------------------------------
-- Duration translation

translateDuration :: SectionInfo -> [Bar pch LyNoteLength anno] -> [Bar pch Duration anno]
translateDuration info = genTransformBars elementD info d_quarter

previousDuration :: DMon Duration
previousDuration = get

setPrevDuration :: Duration -> DMon ()
setPrevDuration d = put d

-- | Spacer and Skip treated differently...
--
elementD :: Element pch LyNoteLength anno -> DMon (Element pch Duration anno)
elementD (Note p d a t)         = (\d1 -> Note p d1 a t) <$> changeDuration d
elementD (Rest d)               = Rest      <$> changeDuration d
elementD (Spacer d)             = Spacer    <$> changeDuration d
elementD (Skip d)               = Rest      <$> skipDuration d
elementD (Chord ps d a t)       = (\d1 -> Chord ps d1 a t) <$> changeDuration d
elementD (Graces ns)            = Graces    <$> mapM grace1D ns
elementD (Punctuation s)        = pure $ Punctuation s

grace1D :: Grace1 pch LyNoteLength -> DMon (Grace1 pch Duration)
grace1D (Grace1 pch drn)        = Grace1 pch <$> changeDuration drn



changeDuration :: LyNoteLength -> DMon Duration
changeDuration (DrnDefault)    = previousDuration
changeDuration (DrnExplicit d) = setPrevDuration d >> return d

skipDuration :: LyNoteLength -> DMon Duration
skipDuration (DrnDefault)    = previousDuration
skipDuration (DrnExplicit d) = return d

