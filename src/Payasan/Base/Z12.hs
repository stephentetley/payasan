{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Z12
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Z12
--
--------------------------------------------------------------------------------

module Payasan.Base.Z12
  ( 
  -- * Datatype
    Z12
  -- * Conversion type class
  , Modulo12(..)
  ) where



newtype Z12 = Z12 Int
  deriving (Eq,Ord)



instance Show Z12 where
  showsPrec p (Z12 i) = showsPrec p i


liftUZ12 :: (Int -> Int) -> Z12 -> Z12
liftUZ12 op (Z12 a) = Z12 $ mod (op a) 12

liftBZ12 :: (Int -> Int -> Int) -> Z12 -> Z12 -> Z12
liftBZ12 op (Z12 a) (Z12 b) = Z12 $ mod (a `op` b) 12

instance Num Z12 where
  (+) = liftBZ12 (+)
  (-) = liftBZ12 (-)
  (*) = liftBZ12 (*)
  negate        = liftUZ12 negate
  fromInteger i = Z12 $ (fromInteger i) `mod` 12
  signum _      = error "Modular numbers are not signed"
  abs _         = error "Modular numbers are not signed"




--------------------------------------------------------------------------------

class Modulo12 a where
  fromZ12 :: Z12 -> a
  toZ12   :: a  -> Z12


instance Modulo12 Int where
  fromZ12 (Z12 i) = i
  toZ12 i = Z12 $ mod i 12

instance Modulo12 Integer where
  fromZ12 (Z12 i) = fromIntegral i
  toZ12 i = Z12 $ fromIntegral $ mod i 12

--------------------------------------------------------------------------------

