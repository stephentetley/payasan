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

  -- * Hughes list
  , H
  , emptyH
  , appendH
  , consH
  , snocH
  , wrapH
  , replicateH
  , toListH
  , fromListH


  )  where



divModS1 :: Integral a => a -> a -> (a,a)
divModS1 x y = let (d,m0) = (x-1) `divMod` y in (d,m0+1)

divS1 :: Integral a => a -> a -> a
divS1 x y = (x-1) `div` y

modS1 :: Integral a => a -> a -> a
modS1 x y = let m0 = (x-1) `mod` y in m0+1


--------------------------------------------------------------------------------
-- Hughes list
-- Should be obsolete...


type H a = [a] -> [a]

emptyH :: H a 
emptyH = id

appendH :: H a -> H a -> H a
appendH f g = f . g

wrapH :: a -> H a 
wrapH a = (a:)

consH :: a -> H a -> H a
consH a f = (a:) . f

snocH :: H a -> a -> H a
snocH f a = f . (a:)

replicateH :: Int -> a -> H a
replicateH i a = fromListH $ replicate i a


toListH :: H a -> [a]
toListH f = f $ []

fromListH :: [a] -> H a
fromListH xs = (xs++)

