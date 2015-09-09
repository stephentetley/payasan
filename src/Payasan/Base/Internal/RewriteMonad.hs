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
  , evalRewriteDefault

  , get
  , gets
  , put
  , puts
  , askGlobal
  , asksGlobal
  , askLocal
  , asksLocal
  , local

  )  where

import Payasan.Base.Internal.CommonSyntax

--------------------------------------------------------------------------------


-- | Rewritelation monad - Reader+Reader+State.
newtype Rewrite st a = Rewrite { 
    getRewrite :: GlobalRenderInfo -> LocalRenderInfo -> st -> (st, a) }

instance Functor (Rewrite st) where
  fmap f ma = Rewrite $ \r1 r2 s -> 
                let (s1,a) = getRewrite ma r1 r2 s in (s1, f a)

instance Applicative (Rewrite st) where
  pure a    = Rewrite $ \_  _  s -> (s,a)
  mf <*> ma = Rewrite $ \r1 r2 s -> 
                let (s1,f) = getRewrite mf r1 r2 s
                    (s2,a) = getRewrite ma r1 r2 s1
                in (s2, f a)

instance Monad (Rewrite st) where
  return    = pure
  ma >>= k  = Rewrite $ \r1 r2 s -> 
                let (s1,a) = getRewrite ma r1 r2 s in getRewrite (k a) r1 r2 s1
 
evalRewrite :: Rewrite st a -> GlobalRenderInfo -> st -> a
evalRewrite ma r s = 
    let (_,a) = getRewrite ma r default_local_info s in a

evalRewriteDefault :: Rewrite st a -> st -> a
evalRewriteDefault ma s = 
    let (_,a) = getRewrite ma default_global_info default_local_info s in a

get :: Rewrite st st
get = Rewrite $ \_ _ s -> (s,s)

gets :: (st -> a) -> Rewrite st a
gets f = Rewrite $ \_ _ s -> (s,f s)

put :: st -> Rewrite st ()
put s = Rewrite $ \_ _ _ -> (s,())

puts :: (st -> st) -> Rewrite st ()
puts f = Rewrite $ \_ _ s -> (f s,())

askGlobal :: Rewrite st GlobalRenderInfo
askGlobal = Rewrite $ \r1 _ s -> (s,r1)

asksGlobal :: (GlobalRenderInfo -> a) -> Rewrite st a
asksGlobal f = Rewrite $ \r1 _ s -> (s,f r1)

askLocal :: Rewrite st LocalRenderInfo
askLocal = Rewrite $ \_ r2 s -> (s,r2)

asksLocal :: (LocalRenderInfo -> a) -> Rewrite st a
asksLocal f = Rewrite $ \_ r2 s -> (s,f r2)

local :: LocalRenderInfo -> Rewrite st a -> Rewrite st a
local r2 ma = Rewrite $ \r1 _ s -> getRewrite ma r1 r2 s
