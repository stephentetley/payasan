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

    MonoPhrase
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
import Payasan.Base.Monophonic.Internal.MonoOutTrans            as MonoOut
import Payasan.Base.Monophonic.Internal.MonoSyntax

import qualified Payasan.Base.Notelist as MAIN

import Text.PrettyPrint.HughesPJ        -- package: pretty


fromABC :: ABCMonoPhrase -> StdMonoPhrase
fromABC  = fromABCWith default_local_info

fromABCWith :: LocalRenderInfo -> ABCMonoPhrase -> StdMonoPhrase
fromABCWith ri = ABCIn.translate . ABCIn.pushLocalRenderInfo ri


outputAsABC :: StdMonoPhrase -> String
outputAsABC = MAIN.outputAsABC . MonoOut.translate


printAsABC :: StdMonoPhrase -> IO ()
printAsABC = MAIN.printAsABC . MonoOut.translate


ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdMonoPhrase -> IO ()
writeAsMIDI path = MAIN.writeAsMIDI path . MonoOut.translate
