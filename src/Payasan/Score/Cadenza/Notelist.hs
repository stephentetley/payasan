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
   
  , StdCadenzaPart
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

import Payasan.Score.Cadenza.Internal.CadenzaToBeam
import Payasan.Score.Cadenza.Internal.InTrans
import Payasan.Score.Cadenza.Internal.Parser
import Payasan.Score.Cadenza.Internal.Syntax


import qualified Payasan.PSC.Backend.LilyPond.OutTrans        as LY
import qualified Payasan.PSC.Backend.LilyPond.SimpleOutput    as LY
import qualified Payasan.PSC.Backend.LilyPond.Utils           as PP

import Payasan.PSC.Repr.IRBeam.AddBeams (noBeams)
import Payasan.PSC.Pipeline (LilyPondPipeline(..))
import qualified Payasan.PSC.Pipeline                         as MAIN
import Payasan.PSC.Shell
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty



fromLilyPond_Relative :: Pitch 
                      -> LyCadenzaPart1 anno 
                      -> StdCadenzaPart1 anno
fromLilyPond_Relative pch = fromLilyPondWith_Relative pch default_section_info

fromLilyPondWith_Relative :: Pitch  
                          -> SectionInfo
                          -> LyCadenzaPart1 anno
                          -> StdCadenzaPart1 anno
fromLilyPondWith_Relative pch locals = 
    lilyPondTranslate_Relative pch . pushSectionInfo (locals { section_meter = Unmetered })



genOutputAsLilyPond :: LilyPondPipeline p1i a1i p1o a1o
                    -> StdCadenzaPart2 p1i a1i
                    -> Doc
genOutputAsLilyPond config = 
    outputStep . toGenLyPart . beamingRewrite . translateToBeam
  where
    beamingRewrite      = beam_trafo config
    toGenLyPart         = out_trafo config
    outputStep          = output_func config



outputAsLilyPond_Relative :: Anno anno 
                          => ScoreInfo -> Pitch -> StdCadenzaPart1 anno -> String
outputAsLilyPond_Relative infos pch = MAIN.ppRender . genOutputAsLilyPond config
  where
    config  = LilyPondPipeline { beam_trafo  = noBeams
                               , out_trafo   = LY.translateToOutput_Relative pch
                               , output_func = LY.simpleScore_Relative std_def infos pch
                               }
    std_def = LY.LyOutputDef { LY.printPitch = PP.pitch, LY.printAnno = anno }

printAsLilyPond_Relative :: Anno anno 
                => ScoreInfo -> Pitch -> StdCadenzaPart1 anno -> IO ()
printAsLilyPond_Relative globals pch = 
    putStrLn . outputAsLilyPond_Relative globals pch

