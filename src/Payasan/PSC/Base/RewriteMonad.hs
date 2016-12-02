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

    RewriteError
  , Rewrite
  , evalRewrite

  , get
  , gets
  , put
  , puts
  , ask
  , asks
  , local
  , throwError
  
  )  where


--------------------------------------------------------------------------------

type RewriteError = String


-- | Rewrite monad - Reader+State.
newtype Rewrite env st a = Rewrite { 
    getRewrite :: env -> st -> Either RewriteError (st, a) }

instance Functor (Rewrite env st) where
  fmap f ma = Rewrite $ \r1 s -> 
                getRewrite ma r1 s >>= \(s1,a) -> return (s1, f a)

instance Applicative (Rewrite env st) where
  pure a    = Rewrite $ \_  s -> return (s,a)
  mf <*> ma = Rewrite $ \r1 s -> 
                getRewrite mf r1 s >>= \(s1,f) ->
                getRewrite ma r1 s1 >>= \(s2,a) ->
                return (s2, f a)

instance Monad (Rewrite env st) where
  return    = pure
  ma >>= k  = Rewrite $ \r1 s -> 
                getRewrite ma r1 s >>= \(s1,a) -> getRewrite (k a) r1 s1
                

-- TODO - is an Alternative instance useful?
 
evalRewrite :: Rewrite env st a -> env -> st -> Either RewriteError a
evalRewrite ma r s = fmap snd $ getRewrite ma r s


get :: Rewrite env st st
get = Rewrite $ \_ s -> return (s,s)

gets :: (st -> a) -> Rewrite env st a
gets f = Rewrite $ \_ s -> return (s,f s)

put :: st -> Rewrite env st ()
put s = Rewrite $ \_ _ -> return (s,())

puts :: (st -> st) -> Rewrite env st ()
puts f = Rewrite $ \_ s -> return (f s,())


ask :: Rewrite env st env
ask = Rewrite $ \r s -> return (s,r)

asks :: (env -> a) -> Rewrite env st a
asks f = Rewrite $ \r s -> return (s,f r)

local :: env -> Rewrite env st a -> Rewrite env st a
local r ma = Rewrite $ \_ s -> getRewrite ma r s

throwError :: RewriteError -> Rewrite env st a
throwError msg = Rewrite $ \_ _ -> Left msg



