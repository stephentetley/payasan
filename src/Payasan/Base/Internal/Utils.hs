{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE CPP                        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Utils
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Hughes lists...
--
-- None of the code in this module should be exposed to clients.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Utils
  ( 

    Trans
  , evalTrans
  , get
  , gets
  , put
  , puts
  , ask 
  , asks

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

  -- * Cat List
  , CatList
  , emptyCat
  , wrapCat
  , appendCat
  , replicateCat
  , toListCat
  , fromListCat

  )  where


import Data.Data
import qualified Data.Foldable as F

#ifndef MIN_VERSION_GLASGOW_HASKELL
import Data.Monoid
#endif
import qualified Data.Traversable as T



--------------------------------------------------------------------------------


-- | Translation monad - Reader+State.
newtype Trans r st a = Trans { getTrans :: r -> st -> (st, a) }

instance Functor (Trans r st) where
  fmap f ma = Trans $ \r s -> let (s1,a) = getTrans ma r s in (s1, f a)

instance Applicative (Trans r st) where
  pure a    = Trans $ \_ s -> (s,a)
  mf <*> ma = Trans $ \r s -> 
                let (s1,f) = getTrans mf r s
                    (s2,a) = getTrans ma r s1
                in (s2, f a)

instance Monad (Trans r st) where
  return    = pure
  ma >>= k  = Trans $ \r s -> 
                let (s1,a) = getTrans ma r s in getTrans (k a) r s1
 
evalTrans :: Trans r st a -> r -> st -> a
evalTrans ma r s = let (_,a) = getTrans ma r s in a

get :: Trans r st st
get = Trans $ \_ s -> (s,s)

gets :: (st -> a) -> Trans r st a
gets f = Trans $ \_ s -> (s,f s)

put :: st -> Trans r st ()
put s = Trans $ \_ _ -> (s,())

puts :: (st -> st) -> Trans r st ()
puts f = Trans $ \_ s -> (f s,())

ask :: Trans r st r
ask = Trans $ \r s -> (s,r)

asks :: (r -> a) -> Trans r st a
asks f = Trans $ \r s -> (s,f r)


--------------------------------------------------------------------------------
-- Hughes list


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


--------------------------------------------------------------------------------
-- Cat list list that supports efficient concat but also Data+Typeable

data CatList a = None 
               | Single [a]
               | Concat (CatList a) (CatList a)
  deriving (Data,Eq,Functor,Ord,Show,Typeable)


instance Monoid (CatList a) where
  mempty  = emptyCat
  mappend = appendCat

instance F.Foldable CatList where
  foldMap _ (None)        = mempty
  foldMap f (Single xs)   = F.foldMap f xs
  foldMap f (Concat x y)  = F.foldMap f x `mappend` F.foldMap f y


instance T.Traversable CatList where
  traverse _ (None)       = pure None
  traverse f (Single xs)  = Single <$> T.traverse f xs
  traverse f (Concat x y) = Concat <$> T.traverse f x <*> T.traverse f y


emptyCat :: CatList a
emptyCat = None

wrapCat :: [a] -> CatList a
wrapCat = Single

appendCat :: CatList a -> CatList a -> CatList a
appendCat a      (None) = a
appendCat (None) b      = b
appendCat a      b      = Concat a b

replicateCat :: Int -> CatList a -> CatList a
replicateCat i _  | i < 0  = emptyCat
replicateCat i xs | i == 1 = xs
replicateCat i xs          = step $ replicate i xs
  where
    step []                 = emptyCat
    step [ys]               = ys
    step (ys:yss)           = ys `appendCat` step yss


toListCat :: CatList a -> [a]
toListCat (None)       = []
toListCat (Single xs)  = xs
toListCat (Concat a b) = let ys = toListCat b in toListCat a ++ ys

fromListCat :: [a] -> CatList a
fromListCat = Single 


