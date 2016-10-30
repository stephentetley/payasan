{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Base.RewriteMonad
-- Copyright   :  (c) Stephen Tetley 2014-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Rewrite Monad
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Base.RewriteMonad
  ( 

    Rewrite
  , evalRewrite

  , get
  , gets
  , put
  , puts
  , ask
  , asks
  , local

  )  where

import Payasan.PSC.Base.SyntaxCommon

--------------------------------------------------------------------------------


-- | Rewrite monad - Reader+State.
newtype Rewrite st a = Rewrite { 
    getRewrite :: SectionInfo -> st -> (st, a) }

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
    let (_,a) = getRewrite ma default_section_info s in a


get :: Rewrite st st
get = Rewrite $ \_ s -> (s,s)

gets :: (st -> a) -> Rewrite st a
gets f = Rewrite $ \_ s -> (s,f s)

put :: st -> Rewrite st ()
put s = Rewrite $ \_ _ -> (s,())

puts :: (st -> st) -> Rewrite st ()
puts f = Rewrite $ \_ s -> (f s,())


ask :: Rewrite st SectionInfo
ask = Rewrite $ \r s -> (s,r)

asks :: (SectionInfo -> a) -> Rewrite st a
asks f = Rewrite $ \r s -> (s,f r)

local :: SectionInfo -> Rewrite st a -> Rewrite st a
local r ma = Rewrite $ \_ s -> getRewrite ma r s
