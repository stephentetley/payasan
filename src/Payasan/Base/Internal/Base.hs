{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Base
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Base
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Base
  ( 

    Decimal
  , Seconds
  , BPM


  )  where


import Data.Fixed

type Decimal = Fixed E9


type Seconds = Decimal

type BPM     = Decimal



