{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Notelist
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Score represenation for Cadenza (free metered) music.
--
--------------------------------------------------------------------------------

module Payasan.Score.Cadenza.Notelist
  ( 

    module Payasan.PSC.Shell
   
  , StdCadenzaSection
  , cadenza

  , ScoreInfo(..)        -- Re-export
  , default_score_info


  , SectionInfo(..)         -- Re-export
  , UnitNoteLength(..)
  , default_section_info

  , fromLilyPond_Relative
  , fromLilyPondWith_Relative

  , genOutputAsLilyPond

  , outputAsLilyPond_Relative
  , printAsLilyPond_Relative

  ) where

import Payasan.Score.Cadenza.Internal.CadenzaToExternal
import Payasan.Score.Cadenza.Internal.InTrans
import Payasan.Score.Cadenza.Internal.Parser
import Payasan.Score.Cadenza.Internal.Syntax


import qualified Payasan.PSC.LilyPond.OutTrans        as LY
import qualified Payasan.PSC.LilyPond.SimpleOutput    as LY
import qualified Payasan.PSC.LilyPond.Utils           as PP

import Payasan.PSC.Repr.External.AddBeams (noBeams)
import Payasan.PSC.Pipeline (LilyPondPipeline(..))
import qualified Payasan.PSC.Pipeline                         as MAIN
import Payasan.PSC.Shell
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Basis
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty



fromLilyPond_Relative :: Pitch 
                      -> LyCadenzaSection1 anno 
                      -> StdCadenzaSection1 anno
fromLilyPond_Relative pch = fromLilyPondWith_Relative pch default_section_info

fromLilyPondWith_Relative :: Pitch  
                          -> SectionInfo
                          -> LyCadenzaSection1 anno
                          -> StdCadenzaSection1 anno
fromLilyPondWith_Relative pch locals = 
    lilyPondTranslate_Relative pch . pushSectionInfo (locals { section_meter = Unmetered })



genOutputAsLilyPond :: LilyPondPipeline p1i a1i p1o a1o
                    -> String
                    -> StdCadenzaSection2 p1i a1i
                    -> Doc
genOutputAsLilyPond config name = 
    outputStep . toGenLySection . beamingRewrite . transCadenzaToExternal name
  where
    beamingRewrite      = beam_trafo config
    toGenLySection      = out_trafo config
    outputStep          = output_func config



outputAsLilyPond_Relative :: Anno anno 
                          => ScoreInfo -> String -> Pitch -> StdCadenzaSection1 anno -> String
outputAsLilyPond_Relative infos name pch = MAIN.ppRender . genOutputAsLilyPond config name
  where
    config  = LilyPondPipeline { beam_trafo  = noBeams
                               , out_trafo   = LY.translateToOutput_Relative pch
                               , output_func = LY.simpleScore_Relative std_def infos pch
                               }
    std_def = LY.LyOutputDef { LY.printPitch = PP.pitch, LY.printAnno = anno }

printAsLilyPond_Relative :: Anno anno 
                         => ScoreInfo -> String -> Pitch -> StdCadenzaSection1 anno -> IO ()
printAsLilyPond_Relative globals name pch = 
    putStrLn . outputAsLilyPond_Relative globals name pch

