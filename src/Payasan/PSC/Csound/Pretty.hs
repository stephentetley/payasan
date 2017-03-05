{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Csound.Pretty
-- Copyright   :  (c) Stephen Tetley 2016-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pretty printers for Csound output.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Csound.Pretty
  ( 

    comment 
  , textp
  , boolp
  , intp
  , doublep
  , decimalp
  , stringp

  ) where


import Payasan.Base.Basis

import Text.PrettyPrint.HughesPJ                -- package: pretty

import Numeric


comment :: String -> Doc
comment ss = text $ "; " ++ ss

textp :: Int -> String -> Doc
textp i ss = text $ ss ++ replicate (i - length ss) ' '

boolp :: Int -> Bool -> Doc
boolp w True    = intp w 1
boolp w False   = intp w 0

intp :: Int -> Int -> Doc    
intp w i        = textp w (show i)


doublep :: (Int,Int) -> Double -> Doc
doublep (w,prec) d = textp w d1
  where
    d1 = ($ "") $ showFFloat (Just prec) d

decimalp :: (Int,Int) -> Decimal -> Doc
decimalp fmt d = doublep fmt $ realToFrac d

-- | Double quoted
stringp :: Int -> String -> Doc
stringp w ss = textp w ('"': ss ++ "\"")