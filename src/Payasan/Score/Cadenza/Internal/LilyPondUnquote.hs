{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.LilyPondUnquote
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert quoted LilyPond to Cadenza Syntax.
--
--------------------------------------------------------------------------------

module Payasan.Score.Cadenza.Internal.LilyPondUnquote
  (

    cadenza_lilypond
  
  , unquoteLyRelative
  , unquoteLyAbsolute
  , unquoteLilyPond
  
  ) where


import Payasan.Score.Cadenza.Internal.LilyPondParser
import Payasan.Score.Cadenza.Internal.Syntax
import Payasan.Score.Cadenza.Internal.Traversals

import Payasan.PSC.LilyPond.Base hiding ( LySectionQuote(..) )

import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.Parsec                              -- package: parsec

import Language.Haskell.TH.Quote                -- package: template-haskell




--------------------------------------------------------------------------------
-- Quasiquote

cadenza_lilypond :: QuasiQuoter
cadenza_lilypond = QuasiQuoter
    { quoteExp = \s -> case parseCadenzaNoAnno s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 




type DMon a    = Mon Duration a
type RelPMon a = Mon Pitch a
type AbsPMon a = Mon () a



unquoteLyRelative :: String 
                  -> SectionInfo 
                  -> Pitch 
                  -> LySectionQuote LyPitch anno 
                  -> Section Pitch Duration anno
unquoteLyRelative name info rpitch (LySectionQuote bs) =
    let groups = trafoDuration info $ trafoRelPitch info rpitch bs
    in Section { section_name      = name
               , section_info      = info
               , section_groups    = groups
               }
               

unquoteLyAbsolute :: String -> SectionInfo -> LySectionQuote LyPitch anno -> Section Pitch Duration anno
unquoteLyAbsolute name info (LySectionQuote bs) =
    let groups = trafoDuration info $ trafoAbsPitch info bs
    in Section { section_name      = name
               , section_info      = info
               , section_groups    = groups
               }

unquoteLilyPond :: String -> SectionInfo -> LySectionQuote pch anno -> Section pch Duration anno
unquoteLilyPond name info (LySectionQuote bs) =
    let groups = trafoDuration info bs
    in Section { section_name      = name
               , section_info      = info
               , section_groups    = groups
               }


--------------------------------------------------------------------------------
-- Relative Pitch translation

trafoRelPitch :: SectionInfo -> Pitch -> [NoteGroup LyPitch drn anno] -> [NoteGroup Pitch drn anno]
trafoRelPitch info p0 = genTransformNoteGroups relElementP info p0


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

trafoAbsPitch :: SectionInfo -> [NoteGroup LyPitch drn anno] -> [NoteGroup Pitch drn anno]
trafoAbsPitch info = genTransformNoteGroups absElementP info ()



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

trafoDuration :: SectionInfo 
              -> [NoteGroup pch LyNoteLength anno] 
              -> [NoteGroup pch Duration anno]
trafoDuration info = genTransformNoteGroups elementD info d_quarter


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

