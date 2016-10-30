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
-- Hughes lists...
--
-- None of the code in this module should be exposed to clients.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Utils
  ( 
    
    ParsecParser
  , ParsecLexer
  , fullInputParse

  , divModS1
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


import Text.Parsec                              -- package: parsec
import Text.Parsec.Token

import Control.Monad.Identity
import Data.Char (isSpace)


type ParsecParser a        = ParsecT String () Identity a
type ParsecLexer           = GenTokenParser String () Identity



fullInputParse :: forall a. ParsecParser () -> ParsecParser a -> ParsecParser a
fullInputParse white p = white *> parseK >>= step
  where 
    isTrail             = all (isSpace)
    step (ans,_,ss) 
        | isTrail ss    = return ans
        | otherwise     = fail $ "parseFail - remaining input: " ++ ss


    parseK :: ParsecParser (a, SourcePos, String)
    parseK = (,,) <$> p <*> getPosition <*> getInput




divModS1 :: Integral a => a -> a -> (a,a)
divModS1 x y = let (d,m0) = (x-1) `divMod` y in (d,m0+1)

divS1 :: Integral a => a -> a -> a
divS1 x y = (x-1) `div` y

modS1 :: Integral a => a -> a -> a
modS1 x y = let m0 = (x-1) `mod` y in m0+1


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

