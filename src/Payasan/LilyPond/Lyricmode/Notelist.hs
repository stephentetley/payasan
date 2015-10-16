{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Lyricmode.Notelist
-- Copyright   :  (c Stephen Tetley 2015
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
   
  , StdLyricPhrase
  , lyricmode

  , ScoreInfo(..)        -- Re-export
  , OctaveMode(..)
  , default_score_info

  , LocalContextInfo(..)         -- Re-export
  , UnitNoteLength(..)
  , default_local_info

  , fromLilyPond
  , fromLilyPondWith

  , outputAsLilyPond
  , printAsLilyPond

  ) where

import Payasan.LilyPond.Lyricmode.Internal.Base
import Payasan.LilyPond.Lyricmode.Internal.Output
import Payasan.LilyPond.Lyricmode.Internal.Parser

import qualified Payasan.Base.Monophonic.Internal.Syntax    as MONO
import qualified Payasan.Base.Monophonic.Notelist           as MONO

import qualified Payasan.Base.Internal.LilyPond.OutTrans    as LY

import Payasan.Base.Internal.AddBeams (noBeams)
import Payasan.Base.Internal.CommonSyntax
import qualified Payasan.Base.Internal.Pipeline             as MAIN
import Payasan.Base.Internal.Shell


-- import Text.PrettyPrint.HughesPJClass           -- package: pretty



fromLilyPond :: ScoreInfo -> LyLyricPhrase -> StdLyricPhrase
fromLilyPond globals = fromLilyPondWith globals default_local_info

fromLilyPondWith :: ScoreInfo 
                 -> LocalContextInfo 
                 -> LyLyricPhrase
                 -> StdLyricPhrase
fromLilyPondWith globals locals = inTrans globals . MONO.pushContextInfo locals


-- This should not beam...
-- Ideally print two simultaneous interpretations...
--
outputAsLilyPond :: ScoreInfo -> StdLyricPhrase -> String
outputAsLilyPond globals = MAIN.ppRender . MONO.genOutputAsLilyPond config
  where
    config  = MONO.LilyPondPipeline 
                { MONO.beam_trafo  = noBeams
                , MONO.out_trafo   = LY.translateToOutput_DurationOnly
                , MONO.output_func = lyricsOutput globals
                }



printAsLilyPond :: ScoreInfo -> StdLyricPhrase -> IO ()
printAsLilyPond gi = putStrLn . outputAsLilyPond gi

