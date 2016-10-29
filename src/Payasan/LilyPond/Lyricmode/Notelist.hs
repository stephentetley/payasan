{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Lyricmode.Notelist
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Lyricmode for LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Lyricmode.Notelist
  ( 

    module Payasan.Base.Internal.Shell
   
  , LyricPart1
  , lyricmode

  , ScoreInfo(..)        -- Re-export
  , default_score_info

  , SectionInfo(..)         -- Re-export
  , UnitNoteLength(..)
  , default_section_info

  , fromLilyPond
  , fromLilyPondWith

  , outputAsLilyPond
  , outputAsLilyPondDU
  , printAsLilyPond

  ) where

import Payasan.LilyPond.Lyricmode.Internal.Base
import Payasan.LilyPond.Lyricmode.Internal.Interpret
import Payasan.LilyPond.Lyricmode.Internal.Output
import Payasan.LilyPond.Lyricmode.Internal.Parser

import qualified Payasan.Score.Elementary.Internal.Syntax       as ELEM
import qualified Payasan.Score.Elementary.Internal.Traversals   as ELEM
import qualified Payasan.Score.Elementary.Notelist              as ELEM

import qualified Payasan.PSC.Backend.LilyPond.OutTrans        as LY

import Payasan.PSC.Repr.IRBeam.AddBeams
import qualified Payasan.Base.Internal.Pipeline                 as MAIN
import Payasan.Base.Internal.Shell
import Payasan.Base.Internal.SyntaxCommon



fromLilyPond :: ScoreInfo -> LyLyricPart -> LyricPart1 ()
fromLilyPond globals = fromLilyPondWith globals default_section_info

fromLilyPondWith :: ScoreInfo 
                 -> SectionInfo 
                 -> LyLyricPart1 anno
                 -> LyricPart1 anno
fromLilyPondWith globals locals = inTrans globals . ELEM.pushSectionInfo locals


-- Lyrics should not beam.
-- Print two simultaneous interpretations.
--
outputAsLilyPond :: Anno anno => ScoreInfo -> LyricPart1 anno -> String
outputAsLilyPond globals lyrics = 
    MAIN.ppRender $ ELEM.genOutputAsLilyPond2 config2 beats lyrics
  where
    beats           = ELEM.censorPunctuation $ ELEM.changeSkipToRest $ extractRhythm lyrics
    config2         = MAIN.LilyPondPipeline2
                        { MAIN.pipe2_beam_trafo1   = addBeams
                        , MAIN.pipe2_out_trafo1    = LY.translateToOutput_Absolute
                        , MAIN.pipe2_beam_trafo2   = noBeams
                        , MAIN.pipe2_out_trafo2    = LY.translateToOutput_DurationOnly
                        , MAIN.pipe2_output_func   = lyricsScore globals
                        }


outputAsLilyPondDU :: AnnoDU anno -> ScoreInfo -> LyricPart1 anno -> String
outputAsLilyPondDU annos globals lyrics = 
    MAIN.ppRender $ ELEM.genOutputAsLilyPond2 config2 beats lyrics
  where
    beats           = ELEM.censorPunctuation $ ELEM.changeSkipToRest $ extractRhythm lyrics
    config2         = MAIN.LilyPondPipeline2
                        { MAIN.pipe2_beam_trafo1   = addBeams
                        , MAIN.pipe2_out_trafo1    = LY.translateToOutput_Absolute
                        , MAIN.pipe2_beam_trafo2   = noBeams
                        , MAIN.pipe2_out_trafo2    = LY.translateToOutput_DurationOnly
                        , MAIN.pipe2_output_func   = lyricsScoreDU annos globals
                        }



printAsLilyPond :: Anno anno => ScoreInfo -> LyricPart1 anno -> IO ()
printAsLilyPond globals = putStrLn . outputAsLilyPond globals

