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
  , lyricsScoreDU
  , rhythmVoice
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
    rhythm          = rhythmVoice anno ph1
    lyrics          = lyricsVoice ph2
                      

lyricsScoreDU :: AnnoDU a
              -> ScoreInfo 
              -> BEAM.Phrase LyPitch LyNoteLength a
              -> BEAM.Phrase Syllable LyNoteLength az
              -> Doc
lyricsScoreDU annos globals ph1 ph2 = 
        header $+$ defs annos $+$ simultaneous1 (rhythm $+$ lyrics)
  where
    header          = scoreHeader globals
    rhythm          = rhythmVoice (use annos) ph1
    lyrics          = lyricsVoice ph2
                      



-- rhythmVoice would be better with an explicit annof printer 
-- than the Anno instance... 

rhythmVoice :: (a -> Doc) -> BEAM.Phrase LyPitch LyNoteLength a -> Doc
rhythmVoice annof ph = newVoiceDefn "rhythm" <+> anonBlock body
  where
    body        = vcat [ hide_ "Staff.StaffSymbol" 
                       , hide_ "Staff.Clef"
                       , numericTimeSignature_
                       , stemDown_
                       , simpleVoice_Absolute def ph
                       ]
    def         = LyOutputDef { printPitch = pitch, printAnno = annof }
                      

lyricsVoice :: BEAM.Phrase Syllable LyNoteLength a -> Doc
lyricsVoice ph = block (Just prefix) (overrides $+$ notes)
  where
    prefix      = command "new" <+> text "Lyrics" <+> command "lyricmode"
    locals1     = maybe default_section_info id $ BEAM.firstContextInfo ph
    overrides   = vcat [ override_ "LyricText #'font-size = #-1"
                       , override_ "Lyrics.LyricSpace.minimum-distance = #1.4"
                       , set_ "associatedVoice = #\"rhythm\""
                       ]         
    notes       = lilypondNotes lyric_def locals1 ph
    lyric_def   = LyOutputDef { printPitch = pPrint, printAnno = \_ -> empty }

