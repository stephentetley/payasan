{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Notelist
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Fret diagrams for LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.FretDiagram.Notelist
  ( 
   
    module Payasan.Base.Internal.Shell
  , module Payasan.LilyPond.FretDiagram.Internal.Plain

  , ScoreInfo(..)               -- Re-export
  , OctaveMode(..)
  , default_score_info

  , LocalContextInfo(..)        -- Re-export
  , UnitNoteLength(..)
  , default_local_info

  , FretDiagram                 -- Re-export
  , pushName
  , GuitarTuning
  , standard_tuning
  , fret_diagram

  , MAIN.LilyPondPipeline(..)
  , outputAsLilyPond
  , printAsLilyPond

  ) where

import Payasan.LilyPond.FretDiagram.Internal.Base
import Payasan.LilyPond.FretDiagram.Internal.Output
import Payasan.LilyPond.FretDiagram.Internal.Parser
import Payasan.LilyPond.FretDiagram.Internal.Plain

import Payasan.Base.Internal.AddBeams
import qualified Payasan.Base.Internal.LilyPond.OutTrans      as LY
import qualified Payasan.Base.Monophonic.Internal.MonoToMain  as MONO


import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.Shell

import qualified Payasan.Base.Notelist as MAIN



-- No @fromLilyPond@ function as Fret diagrams are markup.
-- Maybe we should have a qq syntax anyway...


outputAsLilyPond :: ScoreInfo -> [FretDiagram] -> FretDiagramPhrase -> String
outputAsLilyPond globals defs = 
    MAIN.ppRender . MAIN.genOutputAsLilyPond config . MONO.chordTranslateToMain 
  where
    config  = MAIN.LilyPondPipeline 
                { MAIN.beam_trafo  = addBeams
                , MAIN.out_trafo   = LY.translateToOutput_Absolute
                , MAIN.output_func = fretDiagramOutput globals defs
                }



printAsLilyPond :: ScoreInfo -> [FretDiagram] -> FretDiagramPhrase -> IO ()
printAsLilyPond info defs = putStrLn . outputAsLilyPond info defs 

