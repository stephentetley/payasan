{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Notelist
-- Copyright   :  (c) Stephen Tetley 2015-2016
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
   
    module Payasan.PSC.Shell
  , module Payasan.LilyPond.FretDiagram.Internal.Plain

  , ScoreInfo(..)               -- Re-export
  , default_score_info

  , SectionInfo(..)        -- Re-export
  , UnitNoteLength(..)
  , default_section_info

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

import Payasan.PSC.Repr.External.AddBeams
import qualified Payasan.PSC.LilyPond.OutTrans      as LY
import Payasan.Score.Elementary.Internal.ElementaryToExternal


import Payasan.PSC.Shell
import Payasan.PSC.Base.SyntaxCommon

import qualified Payasan.PSC.Pipeline as MAIN



-- No @fromLilyPond@ function as Fret diagrams are markup.
-- Maybe we should have a qq syntax anyway...


outputAsLilyPond :: ScoreInfo -> String -> [FretDiagram] -> FretDiagramSection -> String
outputAsLilyPond globals name diags = 
    MAIN.ppRender . MAIN.genOutputAsLilyPond config . chord_transElementaryToExternal name
  where
    config  = MAIN.LilyPondPipeline 
                { MAIN.beam_trafo  = addBeams
                , MAIN.out_trafo   = LY.translateToOutput_Absolute
                , MAIN.output_func = fretDiagramOutput globals diags
                }



printAsLilyPond :: ScoreInfo -> String -> [FretDiagram] -> FretDiagramSection -> IO ()
printAsLilyPond info name diags = putStrLn . outputAsLilyPond info name diags

