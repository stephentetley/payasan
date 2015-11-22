{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Cadenza.Notelist
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Cadenza for LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Cadenza.Notelist
  ( 

    module Payasan.Base.Internal.Shell
   
  , StdCadenzaPhrase
  , cadenza

  , ScoreInfo(..)        -- Re-export
  , default_score_info


  , LocalContextInfo(..)         -- Re-export
  , UnitNoteLength(..)
  , default_local_info
{-
  , fromLilyPond
  , fromLilyPondWith

  , outputAsLilyPond
  , outputAsLilyPondDU
  , printAsLilyPond
-}
  ) where

import Payasan.LilyPond.Cadenza.Internal.CadenzaToBeam
import Payasan.LilyPond.Cadenza.Internal.InTrans
import Payasan.LilyPond.Cadenza.Internal.Parser
import Payasan.LilyPond.Cadenza.Internal.Syntax


import qualified Payasan.Base.Internal.LilyPond.OutTrans        as LY

import Payasan.Base.Internal.CommonSyntax
import qualified Payasan.Base.Internal.Pipeline                 as MAIN
import Payasan.Base.Internal.Shell

import Payasan.Base.Pitch



fromLilyPond :: ScoreInfo -> Pitch -> LyCadenzaPhrase1 anno -> StdCadenzaPhrase1 anno
fromLilyPond globals = fromLilyPondWith globals default_local_info

fromLilyPondWith :: ScoreInfo 
                 -> LocalContextInfo
                 -> Pitch 
                 -> LyCadenzaPhrase1 anno
                 -> StdCadenzaPhrase1 anno
fromLilyPondWith globals locals pch = lilyPondTranslate_Relative pch . pushContextInfo locals


{-
-- Lyrics should not beam.
-- Print two simultaneous interpretations.
--
outputAsLilyPond :: Anno anno => ScoreInfo -> LyricPhrase1 anno -> String
outputAsLilyPond globals lyrics = 
    MAIN.ppRender $ MONO.genOutputAsLilyPond2 config2 beats lyrics
  where
    beats           = MONO.censorPunctuation $ MONO.skipToRest $ extractRhythm lyrics
    config2         = MAIN.LilyPondPipeline2
                        { MAIN.pipe2_beam_trafo1   = addBeams
                        , MAIN.pipe2_out_trafo1    = LY.translateToOutput_Absolute
                        , MAIN.pipe2_beam_trafo2   = noBeams
                        , MAIN.pipe2_out_trafo2    = LY.translateToOutput_DurationOnly
                        , MAIN.pipe2_output_func   = lyricsScore globals
                        }


outputAsLilyPondDU :: AnnoDU anno -> ScoreInfo -> LyricPhrase1 anno -> String
outputAsLilyPondDU annos globals lyrics = 
    MAIN.ppRender $ MONO.genOutputAsLilyPond2 config2 beats lyrics
  where
    beats           = MONO.censorPunctuation $ MONO.skipToRest $ extractRhythm lyrics
    config2         = MAIN.LilyPondPipeline2
                        { MAIN.pipe2_beam_trafo1   = addBeams
                        , MAIN.pipe2_out_trafo1    = LY.translateToOutput_Absolute
                        , MAIN.pipe2_beam_trafo2   = noBeams
                        , MAIN.pipe2_out_trafo2    = LY.translateToOutput_DurationOnly
                        , MAIN.pipe2_output_func   = lyricsScoreDU annos globals
                        }



printAsLilyPond :: Anno anno => ScoreInfo -> LyricPhrase1 anno -> IO ()
printAsLilyPond globals = putStrLn . outputAsLilyPond globals

-}