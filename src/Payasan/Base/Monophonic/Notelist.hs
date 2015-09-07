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

    Phrase
  , StdMonoPhrase
  , ABCMonoPhrase           -- * re-export
  , abc                     -- * re-export


  , LocalRenderInfo(..)
  , UnitNoteLength(..)
  , default_local_info


  , fromABC
  , fromABCWith
  , outputAsABC
  , printAsABC

  , ppRender

  , writeAsMIDI

  ) where

import qualified Payasan.Base.Monophonic.Internal.ABCInTrans    as ABCIn
import Payasan.Base.Monophonic.Internal.ABCParser (abc)
import Payasan.Base.Monophonic.Internal.MonoToMain
import Payasan.Base.Monophonic.Internal.Syntax

import qualified Payasan.Base.Notelist as MAIN

import Text.PrettyPrint.HughesPJ        -- package: pretty


fromABC :: ABCMonoPhrase -> StdMonoPhrase
fromABC  = fromABCWith default_local_info

fromABCWith :: LocalRenderInfo -> ABCMonoPhrase -> StdMonoPhrase
fromABCWith ri = ABCIn.translate . ABCIn.pushLocalRenderInfo ri


outputAsABC :: StdMonoPhrase -> String
outputAsABC = MAIN.outputAsABC . translateToMain


printAsABC :: StdMonoPhrase -> IO ()
printAsABC = MAIN.printAsABC . translateToMain


ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdMonoPhrase -> IO ()
writeAsMIDI path = MAIN.writeAsMIDI path . translateToMain
