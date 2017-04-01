{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Notelist
-- Copyright   :  (c) Stephen Tetley 2015-2017
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
   
    module Payasan.PSC.Old.Shell
  , module Payasan.LilyPond.FretDiagram.Internal.Plain


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


import Payasan.PSC.Old.Shell
import Payasan.PSC.Base.SyntaxCommon

import qualified Payasan.PSC.Old.Pipeline as MAIN



-- No @fromLilyPond@ function as Fret diagrams are markup.
-- Maybe we should have a qq syntax anyway...


outputAsLilyPond :: String -> String -> String -> [FretDiagram] -> FretDiagramSection -> String
outputAsLilyPond lyversion title name diags = 
    MAIN.ppRender . MAIN.genOutputAsLilyPond config . chord_transElementaryToExternal
  where
    config  = MAIN.LilyPondPipeline 
                { MAIN.beam_trafo  = addBeams
                , MAIN.out_trafo   = LY.translateToLyPartOut_Absolute
                , MAIN.output_func = fretDiagramOutput lyversion title diags
                }



printAsLilyPond :: String -> String -> String -> [FretDiagram] -> FretDiagramSection -> IO ()
printAsLilyPond lyversion title name diags = putStrLn . outputAsLilyPond lyversion title name diags

