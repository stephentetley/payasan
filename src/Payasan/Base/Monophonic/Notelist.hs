{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Notelist
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

module Payasan.Base.Monophonic.Notelist
  ( 

    module Payasan.Base.Monophonic.Internal.Transform

  , Phrase
  , StdMonoPhrase
  , ABCMonoPhrase
  , abc
  , LilyPondMonoPhrase
  , lilypond

  , GlobalRenderInfo(..)
  , OctaveMode(..)
  , default_global_info

  , LocalRenderInfo(..)
  , UnitNoteLength(..)
  , default_local_info


  , fromABC
  , fromABCWith
  , fromLilyPond
  , fromLilyPondWith

  , outputAsABC
  , printAsABC
  , outputAsLilyPond
  , printAsLilyPond

  , ppRender

  , writeAsMIDI

  , mapPch
  , mapDrn

  ) where

import Payasan.Base.Monophonic.Internal.ABCInTrans
import Payasan.Base.Monophonic.Internal.ABCParser (abc)
import Payasan.Base.Monophonic.Internal.LilyPondInTrans
import Payasan.Base.Monophonic.Internal.LilyPondQuasiquote (lilypond)
import Payasan.Base.Monophonic.Internal.MonoToMain
import Payasan.Base.Monophonic.Internal.MonoDurationTrafo
import Payasan.Base.Monophonic.Internal.MonoPitchTrafo
import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Monophonic.Internal.Transform

import qualified Payasan.Base.Notelist as MAIN

import Text.PrettyPrint.HughesPJ        -- package: pretty


fromABC :: ABCMonoPhrase -> StdMonoPhrase
fromABC  = fromABCWith default_local_info

fromABCWith :: LocalRenderInfo -> ABCMonoPhrase -> StdMonoPhrase
fromABCWith ri = abcTranslate . pushLocalRenderInfo ri


fromLilyPond :: GlobalRenderInfo -> LilyPondMonoPhrase -> StdMonoPhrase
fromLilyPond gi = fromLilyPondWith gi default_local_info

fromLilyPondWith :: GlobalRenderInfo 
                 -> LocalRenderInfo 
                 -> LilyPondMonoPhrase 
                 -> StdMonoPhrase
fromLilyPondWith gi ri = lilyPondTranslate gi . pushLocalRenderInfo ri


outputAsABC :: GlobalRenderInfo -> StdMonoPhrase -> String
outputAsABC gi = MAIN.outputAsABC gi . translateToMain


printAsABC :: GlobalRenderInfo -> StdMonoPhrase -> IO ()
printAsABC gi = MAIN.printAsABC gi . translateToMain

outputAsLilyPond :: GlobalRenderInfo -> StdMonoPhrase -> String
outputAsLilyPond gi = MAIN.outputAsLilyPond gi . translateToMain


printAsLilyPond :: GlobalRenderInfo -> StdMonoPhrase -> IO ()
printAsLilyPond gi = MAIN.printAsLilyPond gi . translateToMain

ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdMonoPhrase -> IO ()
writeAsMIDI path = MAIN.writeAsMIDI path . translateToMain
