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
  , lyricsCombine

  ) where

import Payasan.LilyPond.Lyricmode.Internal.Base

import Payasan.Base.Internal.LilyPond.SimpleOutput
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import qualified Payasan.Base.Internal.BeamSyntax as BEAM
import Payasan.Base.Internal.CommonSyntax


import Text.PrettyPrint.HughesPJClass           -- package: pretty


lyricsScore :: ScoreInfo -> BEAM.Phrase Syllable LyNoteLength a -> Doc
lyricsScore globals ph = 
        header
    $+$ anonBlock (lyricsVoice globals ph)
  where
    header          = scoreHeader globals




lyricsVoice :: ScoreInfo -> BEAM.Phrase Syllable LyNoteLength a -> Doc
lyricsVoice _globals ph = block (Just prefix) notes
  where
    prefix      = command "new" <+> text "Lyrics" <+> command "lyricmode"
    notes       = renderNotes lyric_def ph
    lyric_def   = LyOutputDef { printPitch = pPrint, printAnno = \_ -> empty }


-- This is really the wrong type should be Phrases not Docs for input...

lyricsCombine :: ScoreInfo -> Doc -> Doc -> Doc
lyricsCombine info rhythmn lyrics = 
    scoreHeader info $+$ simultaneous1 (rhythmn $+$ lyrics)
