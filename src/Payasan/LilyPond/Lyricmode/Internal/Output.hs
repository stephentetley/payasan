{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Lyricmode.Internal.Output
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Lyricmode output.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Lyricmode.Internal.Output
  ( 
   
    lyricsScore
  , lyricsVoice

  ) where

import Payasan.LilyPond.Lyricmode.Internal.Base

import Payasan.Base.Internal.LilyPond.RhythmicMarkup
import Payasan.Base.Internal.LilyPond.SimpleOutput
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import qualified Payasan.Base.Internal.BeamSyntax as BEAM
import Payasan.Base.Internal.CommonSyntax


import Text.PrettyPrint.HughesPJClass           -- package: pretty


lyricsScore :: Anno a1 
            => ScoreInfo 
            -> BEAM.Phrase LyPitch LyNoteLength a1 
            -> BEAM.Phrase Syllable LyNoteLength a2 
            -> Doc
lyricsScore globals ph1 ph2 = 
        header $+$ simultaneous1 (rhythm $+$ lyrics)
  where
    header          = scoreHeader globals
    rhythm          = rhythmicMarkupVoice rhythm_def voice ph1
    voice           = default_voice_info { voice_ly_octave_mode = AbsPitch }
    rhythm_def      = LyOutputDef { printPitch = pitch, printAnno = anno }

    lyrics          = lyricsVoice globals ph2
                      




lyricsVoice :: ScoreInfo -> BEAM.Phrase Syllable LyNoteLength a -> Doc
lyricsVoice _globals ph = block (Just prefix) notes
  where
    prefix      = command "new" <+> text "Lyrics" <+> command "lyricmode"
    notes       = renderNotes lyric_def ph
    lyric_def   = LyOutputDef { printPitch = pPrint, printAnno = \_ -> empty }

