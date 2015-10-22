{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Percussion.Notelist
-- Copyright   :  (c) Stephen Tetley 2015
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

  , StdDrumPhrase
  , drums

  , ScoreInfo(..)
  , OctaveMode(..)
  , default_score_info

  , LocalContextInfo(..)
  , UnitNoteLength(..)
  , default_local_info


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
import Payasan.Base.Internal.CommonSyntax
import qualified Payasan.Base.Internal.BeamSyntax as BEAM
import Payasan.Base.Internal.MainToBeam
import Payasan.Base.Internal.Shell

import qualified Payasan.Base.Internal.LilyPond.InTrans     as LY
import qualified Payasan.Base.Internal.LilyPond.OutTrans    as LY

import Payasan.Base.Internal.Output.Common
import Payasan.Base.Internal.Output.Linear.OutputMain
import Payasan.Base.Internal.Output.Tabular.OutputMain

import qualified Payasan.Base.Internal.MIDI.BeamToMIDI      as MIDI
import qualified Payasan.Base.Internal.MIDI.OutTrans        as MIDI
import qualified Payasan.Base.Internal.MIDI.RenderOutput    as MIDI
import qualified Payasan.Base.Internal.MIDI.PrimitiveSyntax as MIDI

import qualified Payasan.Base.Notelist as MAIN
import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJClass           -- package: pretty



fromLilyPond :: ScoreInfo -> LyDrumPhrase -> StdDrumPhrase
fromLilyPond gi = fromLilyPondWith gi default_local_info

fromLilyPondWith :: ScoreInfo 
                 -> LocalContextInfo 
                 -> LyDrumPhrase
                 -> StdDrumPhrase
fromLilyPondWith _gi ri = 
    translateToMain . LY.translateFromInput_DurationOnly . BEAM.pushContextInfo ri



outputAsLilyPond :: ScoreInfo -> StdDrumPhrase -> String
outputAsLilyPond globals = MAIN.ppRender . MAIN.genOutputAsLilyPond config
  where
    config  = MAIN.LilyPondPipeline 
                { MAIN.beam_trafo  = addBeams
                , MAIN.out_trafo   = LY.translateToOutput_DurationOnly
                , MAIN.output_func = drumsOutput globals 
                }


printAsLilyPond :: ScoreInfo -> StdDrumPhrase -> IO ()
printAsLilyPond gi = putStrLn . outputAsLilyPond gi


ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdDrumPhrase -> IO ()
writeAsMIDI path notes = 
   let trk = MIDI.translateToMIDI (MIDI.simpleTrackData 9) (noteTrans notes)
   in MIDI.writeMF1 path [trk]


noteTrans :: StdDrumPhrase -> BEAM.Phrase MIDI.MidiPitch RDuration Accent
noteTrans = MIDI.translateToMidiD . PERC.translate . translateToBeam




outputAsTabular :: ScoreInfo -> StdDrumPhrase -> String
outputAsTabular _gi ph = ppRender $ mainTabular lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsTabular :: ScoreInfo -> StdDrumPhrase ->  IO ()
printAsTabular gi = putStrLn . outputAsTabular gi




outputAsLinear ::  ScoreInfo -> StdDrumPhrase -> String
outputAsLinear _gi ph = ppRender $ mainLinear lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsLinear :: ScoreInfo -> StdDrumPhrase ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi
