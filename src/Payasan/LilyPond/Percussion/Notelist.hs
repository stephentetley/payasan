{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Percussion.Notelist
-- Copyright   :  (c) Stephen Tetley 2015-2017
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

    module Payasan.PSC.Old.Shell

  , StdDrumPart
  , drums

  , ScoreInfo(..)
  , default_score_info


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

import qualified Payasan.PSC.Repr.External.AddBeams           as BEAM

import qualified Payasan.PSC.MIDI.BeamToMIDI        as MIDI
import qualified Payasan.PSC.MIDI.Output            as MIDI
import qualified Payasan.PSC.MIDI.PrimitiveSyntax   as MIDI


import Payasan.PSC.Old.Shell
import Payasan.PSC.Base.SyntaxCommon


import qualified Payasan.PSC.Repr.External.LilyPondInTrans      as LY
import qualified Payasan.PSC.LilyPond.OutTrans          as LYOut

import Payasan.PSC.Base.ShowCommon
import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.ShowLinear
import Payasan.PSC.Repr.External.ShowTabular

import qualified Payasan.PSC.Old.Pipeline                           as MAIN



import Text.PrettyPrint.HughesPJClass           -- package: pretty

-- TODO - remove at some point
transExternalToIRBeam :: a -> a
transExternalToIRBeam = id

fromLilyPond :: LyDrumPart -> StdDrumPart
fromLilyPond = fromLilyPondWith default_section_info

fromLilyPondWith :: SectionInfo 
                 -> LyDrumPart
                 -> StdDrumPart
fromLilyPondWith locals = 
    LY.translateFromInput_DurationOnly . pushSectionInfo locals



outputAsLilyPond :: ScoreInfo -> StdDrumPart -> String
outputAsLilyPond globals = MAIN.ppRender . MAIN.genOutputAsLilyPond config
  where
    config  = MAIN.LilyPondPipeline 
                { MAIN.beam_trafo  = BEAM.addBeams
                , MAIN.out_trafo   = LYOut.translateToLyPartOut_DurationOnly
                , MAIN.output_func = drumsOutput globals 
                }

printAsLilyPond :: ScoreInfo -> StdDrumPart -> IO ()
printAsLilyPond gi = putStrLn . outputAsLilyPond gi


ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdDrumPart -> IO ()
writeAsMIDI path ph = 
   let notes = PERC.translate $ transExternalToIRBeam ph
       trk   = MIDI.translateToMIDI (MIDI.simpleTrackData 9) notes
   in MIDI.writeMF1 path [trk]





outputAsTabular :: ScoreInfo -> StdDrumPart -> String
outputAsTabular _gi ph = ppRender $ mainTabular lo ph
  where
    lo = LeafOutputNote { pp_pitch     = pPrint
                        , pp_duration  = pPrint
                        , pp_anno      = const empty
                        }

printAsTabular :: ScoreInfo -> StdDrumPart ->  IO ()
printAsTabular gi = putStrLn . outputAsTabular gi




outputAsLinear ::  ScoreInfo -> StdDrumPart -> String
outputAsLinear _gi ph = ppRender $ mainLinear lo ph
  where
    lo = LeafOutputNote { pp_pitch     = pPrint
                        , pp_duration  = pPrint
                        , pp_anno      = const empty
                        }

printAsLinear :: ScoreInfo -> StdDrumPart ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi
