{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.RewriteMonad
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Rewrite Monad
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.RewriteMonad
  ( 

    Rewrite
  , evalRewrite

  , get
  , gets
  , put
  , puts
  , askLocal
  , asksLocal
  , local

  )  where

import Payasan.Base.Internal.CommonSyntax

--------------------------------------------------------------------------------


-- | Rewrite monad - Reader+State.
newtype Rewrite st a = Rewrite { 
    getRewrite :: LocalContextInfo -> st -> (st, a) }

instance Functor (Rewrite st) where
  fmap f ma = Rewrite $ \r1 s -> 
                let (s1,a) = getRewrite ma r1 s in (s1, f a)

instance Applicative (Rewrite st) where
  pure a    = Rewrite $ \_  s -> (s,a)
  mf <*> ma = Rewrite $ \r1 s -> 
                let (s1,f) = getRewrite mf r1 s
                    (s2,a) = getRewrite ma r1 s1
                in (s2, f a)

instance Monad (Rewrite st) where
  return    = pure
  ma >>= k  = Rewrite $ \r1 s -> 
                let (s1,a) = getRewrite ma r1 s in getRewrite (k a) r1 s1
 
evalRewrite :: Rewrite st a -> st -> a
evalRewrite ma s = 
    let (_,a) = getRewrite ma default_local_info s in a


get :: Rewrite st st
get = Rewrite $ \_ s -> (s,s)

gets :: (st -> a) -> Rewrite st a
gets f = Rewrite $ \_ s -> (s,f s)

put :: st -> Rewrite st ()
put s = Rewrite $ \_ _ -> (s,())

puts :: (st -> st) -> Rewrite st ()
puts f = Rewrite $ \_ s -> (f s,())


askLocal :: Rewrite st LocalContextInfo
askLocal = Rewrite $ \r s -> (s,r)

asksLocal :: (LocalContextInfo -> a) -> Rewrite st a
asksLocal f = Rewrite $ \r s -> (s,f r)

local :: LocalContextInfo -> Rewrite st a -> Rewrite st a
local r ma = Rewrite $ \_ s -> getRewrite ma r s
