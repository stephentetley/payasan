{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Lyricmode.Internal.Output
-- Copyright   :  (c) Stephen Tetley 2015-2017
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

import Payasan.PSC.LilyPond.Base
import Payasan.PSC.LilyPond.Pretty
import Payasan.PSC.LilyPond.SimpleOutput

import qualified Payasan.PSC.Repr.External.Syntax as EXT
import Payasan.PSC.Base.SyntaxCommon


import Text.PrettyPrint.HughesPJClass           -- package: pretty


lyricsScore :: Anno a1 
            => String
            -> String 
            -> EXT.Part LyPitch LyNoteLength a1 
            -> EXT.Part Syllable LyNoteLength a2 
            -> Doc
lyricsScore lyversion title' ph1 ph2 = 
        header $+$ simultaneous1 (rhythm $+$ lyrics)
  where
    header          = scoreHeader lyversion title'
    rhythm          = rhythmVoice anno ph1
    lyrics          = lyricsVoice ph2
                      

lyricsScoreDU :: AnnoDU a
              -> String 
              -> String
              -> EXT.Part LyPitch LyNoteLength a
              -> EXT.Part Syllable LyNoteLength az
              -> Doc
lyricsScoreDU annos lyversion title' ph1 ph2 = 
        header $+$ anno_defs annos $+$ simultaneous1 (rhythm $+$ lyrics)
  where
    header          = scoreHeader lyversion title'
    rhythm          = rhythmVoice (anno_use annos) ph1
    lyrics          = lyricsVoice ph2
                      



-- rhythmVoice would be better with an explicit annof printer 
-- than the Anno instance... 

rhythmVoice :: (a -> Doc) -> EXT.Part LyPitch LyNoteLength a -> Doc
rhythmVoice annof ph = newVoiceDefn "rhythm" <+> anonBlock body
  where
    body        = vcat [ hide_ "Staff.StaffSymbol" 
                       , hide_ "Staff.Clef"
                       , numericTimeSignature_
                       , stemDown_
                       , simpleVoice_OLD def ph
                       ]
    def         = LyOutputDef { printPitch = pitch
                              , printAnno = annof 
                              , partContext = absoluteCtx
                              }
                      

lyricsVoice :: EXT.Part Syllable LyNoteLength a -> Doc
lyricsVoice ph = 
    block (Just prefix) (overrides $+$ extractDoc notes)
  where
    prefix      = command "new" <+> text "Lyrics" <+> command "lyricmode"
    overrides   = vcat [ override_ "LyricText #'font-size = #-1"
                       , override_ "Lyrics.LyricSpace.minimum-distance = #1.4"
                       , set_ "associatedVoice = #\"rhythm\""
                       ]         
    notes       = makeLyNoteList lyric_def ph
    lyric_def   = LyOutputDef { printPitch = pPrint
                              , printAnno = \_ -> empty
                              , partContext = emptyCtx
                              }

