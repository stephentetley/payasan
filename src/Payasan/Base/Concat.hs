{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Concat
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Concatenation
--
--------------------------------------------------------------------------------

module Payasan.Base.Concat
  ( 
    bireplicate
  , mreplicate

  ) where


import Data.Monoid

bireplicate :: Int -> a -> a -> [a]
bireplicate n a b = take n $ cycle [a,b]

-- | 
mreplicate :: Monoid a => Int -> a -> a
mreplicate i = mconcat . replicate i

