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
    rhythm          = rhythmVoice ph1
    lyrics          = lyricsVoice ph2
                      


rhythmVoice :: Anno a => BEAM.Phrase LyPitch LyNoteLength a -> Doc
rhythmVoice ph = newStaff_ <+> anonBlock body
  where
    body        = vcat [ hide_ "Staff.StaffSymbol" 
                       , hide_ "Staff.Clef"
                       , numericTimeSignature_
                       , stemDown_
                       , simpleVoice_Absolute def ph
                       ]
    def         = LyOutputDef { printPitch = pitch, printAnno = anno }
                      

lyricsVoice :: BEAM.Phrase Syllable LyNoteLength a -> Doc
lyricsVoice ph = block (Just prefix) notes
  where
    prefix      = command "new" <+> text "Lyrics" <+> command "lyricmode"
    locals1     = maybe default_local_info id $ BEAM.firstContextInfo ph
    notes       = lilypondNotes lyric_def locals1 ph
    lyric_def   = LyOutputDef { printPitch = pPrint, printAnno = \_ -> empty }

