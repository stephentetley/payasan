{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Percussion.Notelist
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

module Payasan.Percussion.Notelist
  ( 

    StdDrumPhrase
  , drums

  , GlobalRenderInfo(..)
  , OctaveMode(..)
  , default_global_info

  , LocalRenderInfo(..)
  , UnitNoteLength(..)
  , default_local_info


  , fromLilyPond
  , fromLilyPondWith

  , outputAsLilyPond
  , printAsLilyPond

  , ppRender


  ) where

import Payasan.Percussion.Internal.Base
import Payasan.Percussion.Internal.Parser (drums)  -- to re-export

import Payasan.Base.Internal.AddBeams
import Payasan.Base.Internal.BeamToMain
import Payasan.Base.Internal.CommonSyntax
import qualified Payasan.Base.Internal.BeamSyntax as BEAM
import Payasan.Base.Internal.MainToBeam

import Payasan.Base.Internal.LilyPond.InTrans
import Payasan.Base.Internal.LilyPond.Output (lilyPondOutput)
import qualified Payasan.Base.Internal.LilyPond.OutTrans    as LYOut

import qualified Payasan.Base.Notelist as MAIN


import Text.PrettyPrint.HughesPJ        -- package: pretty



fromLilyPond :: GlobalRenderInfo -> LyDrumPhrase -> StdDrumPhrase
fromLilyPond gi = fromLilyPondWith gi default_local_info

fromLilyPondWith :: GlobalRenderInfo 
                 -> LocalRenderInfo 
                 -> LyDrumPhrase 
                 -> StdDrumPhrase
fromLilyPondWith _gi ri = 
    translateToMain . translateDurationOnly . BEAM.pushLocalRenderInfo ri



outputAsLilyPond :: GlobalRenderInfo -> StdDrumPhrase -> String
outputAsLilyPond gi = 
    ppRender . lilyPondOutput gi . LYOut.translateDurationOnly . addBeams . translateToBeam


printAsLilyPond :: GlobalRenderInfo -> StdDrumPhrase -> IO ()
printAsLilyPond gi = putStrLn . outputAsLilyPond gi


ppRender :: Doc -> String
ppRender = MAIN.ppRender


-- writeAsMIDI :: FilePath -> StdMonoPhrase -> IO ()
-- writeAsMIDI path = MAIN.writeAsMIDI path . translateToMain
