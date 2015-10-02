{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Internal.Base
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

module Payasan.LilyPond.FretDiagram.Internal.Base
  ( 

    Fretting
  , Fretting1(..)
  , StringNumber
  , FretNumber(..)

  ) where


import Data.Data


type Fretting = [Fretting1]


data Fretting1 = Barre [StringNumber] FretNumber
               | Fret  StringNumber   FretNumber
  deriving (Data,Eq,Show,Typeable)
 

type StringNumber = Int

data FretNumber = OPEN | MUTED | FretNumber !Int
  deriving (Data,Eq,Show,Typeable)
