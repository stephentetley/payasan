{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Base.Classes
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Type classes
--
--------------------------------------------------------------------------------

module Payasan.Models.Base.Classes
  ( 
  -- * Typeclass for inversion
    Invert(..)

  -- * Multiplication
  , Mult(..)

  ) where


class Invert a where invert :: a -> a



--------------------------------------------------------------------------------

infixl 7 ^*^

class Mult a where
  (^*^) :: a -> a -> a
