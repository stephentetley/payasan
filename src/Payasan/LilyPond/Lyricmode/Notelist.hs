{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Lyricmode.Notelist
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Lyricmode for LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Lyricmode.Notelist
  ( 

    module Payasan.Base.Internal.Shell
   
  , StdLyricPhrase
  , lyricmode

  , ScoreInfo(..)        -- Re-export
  , OctaveMode(..)
  , default_score_info

  , LocalContextInfo(..)         -- Re-export
  , UnitNoteLength(..)
  , default_local_info

  , fromLilyPond
  , fromLilyPondWith

  , outputAsLilyPond
  , printAsLilyPond

  ) where

import Payasan.LilyPond.Lyricmode.Internal.Base
import Payasan.LilyPond.Lyricmode.Internal.Parser

import qualified Payasan.Base.Monophonic.Internal.Syntax as MONO
import qualified Payasan.Base.Monophonic.Notelist        as MONO

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.LilyPond.Output (LyOutputDef(..))
import Payasan.Base.Internal.Shell


import Text.PrettyPrint.HughesPJClass           -- package: pretty



fromLilyPond :: ScoreInfo -> LyLyricPhrase -> StdLyricPhrase
fromLilyPond gi = fromLilyPondWith gi default_local_info

fromLilyPondWith :: ScoreInfo 
                 -> LocalContextInfo 
                 -> LyLyricPhrase
                 -> StdLyricPhrase
fromLilyPondWith gi ri = inTrans gi . MONO.pushContextInfo ri


-- This should not beam...
--
outputAsLilyPond :: ScoreInfo -> StdLyricPhrase -> String
outputAsLilyPond gi = MONO.genOutputAsLilyPond def gi
  where 
    def = LyOutputDef { printPitch = pPrint, printAnno = \_ -> empty }

printAsLilyPond :: ScoreInfo -> StdLyricPhrase -> IO ()
printAsLilyPond gi = putStrLn . outputAsLilyPond gi

