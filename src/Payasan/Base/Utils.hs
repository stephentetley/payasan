{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Utils
-- Copyright   :  (c) Stephen Tetley 2014-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Utility code 
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Utils
  ( 
    
    divModS1
  , divS1
  , modS1

  -- * Z12 number type
  , Z12
    
  -- * Conversion type class
  , Modulo12(..)


  )  where


-- | divMod "starting at 1"
divModS1 :: Integral a => a -> a -> (a,a)
divModS1 x y = let (d,m0) = (x-1) `divMod` y in (d,m0+1)

-- | div "starting at 1"
divS1 :: Integral a => a -> a -> a
divS1 x y = (x-1) `div` y

-- | mod "starting at 1"
modS1 :: Integral a => a -> a -> a
modS1 x y = let m0 = (x-1) `mod` y in m0+1

--------------------------------------------------------------------------------
-- Z12

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

