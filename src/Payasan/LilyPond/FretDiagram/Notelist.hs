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
   
    module Payasan.Base.Internal.Shell
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

import Payasan.PSC.Repr.IRBeam.AddBeams
import qualified Payasan.PSC.Backend.LilyPond.OutTrans      as LY
import qualified Payasan.Score.Elementary.Internal.ElementaryToExternal   as ELEM


import Payasan.Base.Internal.Shell
import Payasan.Base.Internal.SyntaxCommon

import qualified Payasan.Base.Notelist as MAIN



-- No @fromLilyPond@ function as Fret diagrams are markup.
-- Maybe we should have a qq syntax anyway...


outputAsLilyPond :: ScoreInfo -> [FretDiagram] -> FretDiagramPart -> String
outputAsLilyPond globals diags = 
    MAIN.ppRender . MAIN.genOutputAsLilyPond config . ELEM.chordTranslateToMain 
  where
    config  = MAIN.LilyPondPipeline 
                { MAIN.beam_trafo  = addBeams
                , MAIN.out_trafo   = LY.translateToOutput_Absolute
                , MAIN.output_func = fretDiagramOutput globals diags
                }



printAsLilyPond :: ScoreInfo -> [FretDiagram] -> FretDiagramPart -> IO ()
printAsLilyPond info diags = putStrLn . outputAsLilyPond info diags

