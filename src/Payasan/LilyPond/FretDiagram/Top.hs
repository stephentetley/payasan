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
   
--    StdLyricPhrase
--  , lyricmode

    GlobalRenderInfo(..)        -- Re-export
  , OctaveMode(..)
  , default_global_info

  , LocalRenderInfo(..)         -- Re-export
  , UnitNoteLength(..)
  , default_local_info

  ) where

import Payasan.LilyPond.Lyricmode.Internal.Base
import Payasan.LilyPond.Lyricmode.Internal.Parser

import qualified Payasan.Base.Monophonic.Internal.Syntax as MONO
import qualified Payasan.Base.Monophonic.Notelist        as MONO

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.LilyPond.Output (LyOutputDef(..))


import Text.PrettyPrint.HughesPJClass           -- package: pretty


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