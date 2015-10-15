{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Top
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

module Payasan.LilyPond.FretDiagram.Top
  ( 
   
    module Payasan.Base.Internal.Shell
  , module Payasan.LilyPond.FretDiagram.Internal.Plain

  , ScoreInfo(..)        -- Re-export
  , OctaveMode(..)
  , default_score_info

  , LocalContextInfo(..)         -- Re-export
  , UnitNoteLength(..)
  , default_local_info

  , fret_diagram

  , MAIN.LilyPondPipeline(..)
  , outputAsLilyPond

  ) where

import Payasan.LilyPond.FretDiagram.Internal.Base
import Payasan.LilyPond.FretDiagram.Internal.Parser
import Payasan.LilyPond.FretDiagram.Internal.Plain

import qualified Payasan.Base.Monophonic.Internal.Syntax      as MONO
import qualified Payasan.Base.Monophonic.Internal.MonoToMain  as MONO
import qualified Payasan.Base.Monophonic.Notelist             as MONO

import Payasan.Base.Internal.LilyPond.SimpleOutput (LyOutputDef(..))

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.Shell

import qualified Payasan.Base.Notelist as MAIN

import Text.PrettyPrint.HughesPJClass           -- package: pretty


-- No @fromLilyPond@ as Fret boards do not generate a Phrase.
-- Maybe we should have a qq syntax anyway...



outputAsLilyPond :: ScoreInfo
                 -> FretDiagramPhrase
                 -> String
outputAsLilyPond info = 
    MAIN.outputAsLilyPond info . MONO.chordTranslateToMain


{-

fromLilyPond :: GlobalRenderInfo -> LyLyricPhrase -> StdLyricPhrase
fromLilyPond gi = fromLilyPondWith gi default_local_info

fromLilyPondWith :: GlobalRenderInfo 
                 -> LocalRenderInfo 
                 -> LyLyricPhrase
                 -> StdLyricPhrase
fromLilyPondWith gi ri = inTrans gi . MONO.pushLocalRenderInfo ri



outputAsLilyPond :: GlobalRenderInfo -> StdLyricPhrase -> String
outputAsLilyPond gi = MONO.genOutputAsLilyPond def gi
  where 
    def = LyOutputDef { printPitch = pPrint, printAnno = \_ -> empty }

printAsLilyPond :: GlobalRenderInfo -> StdLyricPhrase -> IO ()
printAsLilyPond gi = putStrLn . outputAsLilyPond gi

-}