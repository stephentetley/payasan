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
   
     lyricsOutput    

  ) where

import Payasan.LilyPond.Lyricmode.Internal.Base

import Payasan.Base.Internal.LilyPond.SimpleOutput
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import qualified Payasan.Base.Internal.BeamSyntax as BEAM
import Payasan.Base.Internal.CommonSyntax


import Text.PrettyPrint.HughesPJClass           -- package: pretty


lyricsOutput :: ScoreInfo -> BEAM.Phrase Syllable LyNoteLength a -> Doc
lyricsOutput globals ph = 
        header
    $+$ anonBlock (lyricBlock notes)
  where
    header          = oHeader globals
    notes           = renderNotes lyric_def ph
    lyric_def       = LyOutputDef { printPitch = pPrint
                                  , printAnno  = \_ -> empty }



oHeader :: ScoreInfo -> Doc
oHeader globals  = 
        version (global_ly_version globals)
    $+$ block (Just $ command "header") (title $ global_title globals)


anonBlock :: Doc -> Doc
anonBlock doc  = block Nothing doc

lyricBlock :: Doc -> Doc
lyricBlock doc = block (Just prefix) doc
  where
    prefix = command "new" <+> text "Lyrics" <+> command "lyricmode"

