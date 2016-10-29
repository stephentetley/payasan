{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Backend.Output.Linear.Utils
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Helpers to print in a linear format.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Backend.Output.Linear.Utils
  ( 

    rest
  , spacer
  , skip
  , nullStar
  , duration

  , endBar
  , endPhrase

  , concatBars

  ) where

import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJ        -- package: pretty



barStart :: Int -> Doc
barStart n = char '=' <> int n

rest :: Doc
rest = char 'r'

spacer :: Doc
spacer = char 's'

skip :: Doc
skip = text "skip"

nullStar :: Doc
nullStar = char '*'


duration :: Duration -> Doc
duration d = 
    maybe (int 0) (\(n,dc) -> fn n <> dots dc) $ symbolicComponents d 
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

    
endBar :: Doc
endBar = text "|"

endPhrase :: Doc
endPhrase = text "||"



concatBars :: [Doc] -> Doc
concatBars []     = empty
concatBars (x:xs) = step 1 x xs
  where
    step n b []       = barStart n <+> b <+> endPhrase
    step n b (c:cs)   = barStart n <+> b <+> endBar $+$ (step (n+1) c cs)
