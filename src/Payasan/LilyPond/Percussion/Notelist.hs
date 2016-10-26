{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Percussion.Notelist
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- (Pipeline)
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Percussion.Notelist
  ( 

    module Payasan.Base.Internal.Shell

  , StdDrumPart
  , drums

  , ScoreInfo(..)
  , default_score_info

  , StaffInfo(..)
  , default_staff_info

  , SectionInfo(..)
  , UnitNoteLength(..)
  , default_section_info


  , fromLilyPond
  , fromLilyPondWith

  , outputAsLilyPond
  , printAsLilyPond

  , ppRender

  , writeAsMIDI

  , outputAsTabular
  , printAsTabular

  , outputAsLinear
  , printAsLinear

  ) where

import Payasan.LilyPond.Percussion.Internal.Base
import Payasan.LilyPond.Percussion.Internal.Output (drumsOutput)
import Payasan.LilyPond.Percussion.Internal.Parser (drums)  -- to re-export
import qualified Payasan.LilyPond.Percussion.Internal.PitchTrans    as PERC

import Payasan.Base.Internal.AddBeams
import Payasan.Base.Internal.BeamToMain
import qualified Payasan.Base.Internal.BeamSyntax as BEAM
import Payasan.Base.Internal.MainToBeam
import Payasan.Base.Internal.Shell
import Payasan.Base.Internal.SyntaxCommon

import qualified Payasan.Base.Internal.LilyPond.InTrans     as LY
import qualified Payasan.Base.Internal.LilyPond.OutTrans    as LY

import Payasan.Base.Internal.Output.Common
import Payasan.Base.Internal.Output.Linear.OutputMain
import Payasan.Base.Internal.Output.Tabular.OutputMain

import qualified Payasan.Backend.MIDI.Internal.BeamToMIDI       as MIDI
import qualified Payasan.Backend.MIDI.Internal.Output           as MIDI
import qualified Payasan.Backend.MIDI.Internal.PrimitiveSyntax  as MIDI

import qualified Payasan.Base.Notelist as MAIN

import Text.PrettyPrint.HughesPJClass           -- package: pretty



fromLilyPond :: LyDrumPart -> StdDrumPart
fromLilyPond = fromLilyPondWith default_section_info

fromLilyPondWith :: SectionInfo 
                 -> LyDrumPart
                 -> StdDrumPart
fromLilyPondWith locals = 
    translateToMain . LY.translateFromInput_DurationOnly . BEAM.pushSectionInfo locals



outputAsLilyPond :: ScoreInfo -> StdDrumPart -> String
outputAsLilyPond globals = MAIN.ppRender . MAIN.genOutputAsLilyPond config
  where
    config  = MAIN.LilyPondPipeline 
                { MAIN.beam_trafo  = addBeams
                , MAIN.out_trafo   = LY.translateToOutput_DurationOnly
                , MAIN.output_func = drumsOutput globals 
                }


printAsLilyPond :: ScoreInfo -> StdDrumPart -> IO ()
printAsLilyPond gi = putStrLn . outputAsLilyPond gi


ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdDrumPart -> IO ()
writeAsMIDI path ph = 
   let notes = PERC.translate $ translateToBeam ph
       trk   = MIDI.translateToMIDI (MIDI.simpleTrackData 9) notes
   in MIDI.writeMF1 path [trk]





outputAsTabular :: ScoreInfo -> StdDrumPart -> String
outputAsTabular _gi ph = ppRender $ mainTabular lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsTabular :: ScoreInfo -> StdDrumPart ->  IO ()
printAsTabular gi = putStrLn . outputAsTabular gi




outputAsLinear ::  ScoreInfo -> StdDrumPart -> String
outputAsLinear _gi ph = ppRender $ mainLinear lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsLinear :: ScoreInfo -> StdDrumPart ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi
