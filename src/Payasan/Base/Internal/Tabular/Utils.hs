{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Tabular.Utils
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Helpers to print in Humdrum-like format.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Tabular.Utils
  ( 

    OutMon
  , nextBar
  , (<++>)
  , comment
  , tandem
  , exclusive
  , bar
  , rest
  , nullDot
  , duration
  , endSpine

  ) where

import Payasan.Base.Internal.RewriteMonad
import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJ        -- package: pretty


-- Generating output is stateful to track bar number

type OutMon a = Rewrite Int a


nextBar :: OutMon Doc
nextBar = do { i <- get; puts (1+); return $ bar i }



infixl 6 <++>

(<++>) :: Doc -> Doc -> Doc
a <++> b = a <> sizedText 8 "\t" <> b

comment :: String -> Doc
comment = text . ('!':)

tandem :: Doc -> Doc
tandem d    = char '*' <> d

exclusive :: Doc -> Doc
exclusive d = text "**" <> d


bar :: Int -> Doc
bar n = char '=' <> int n

rest :: Doc
rest = char 'r'

nullDot :: Doc
nullDot = char '.'


duration :: Duration -> Doc
duration d = 
    maybe (comment "zero") (\(n,dc) -> fn n <> dots dc) $ symbolicComponents d 
  where
    dots dc     = text $ replicate dc '.'
    fn Maxima   = text "maxima"
    fn Longa    = text "longa"
    fn Breve    = text "breve"
    fn D1       = int 1
    fn D2       = int 2
    fn D4       = int 4
    fn D8       = int 8
    fn D16      = int 16
    fn D32      = int 32
    fn D64      = int 64
    fn D128     = int 128

    
endSpine :: Doc
endSpine = text "*-"