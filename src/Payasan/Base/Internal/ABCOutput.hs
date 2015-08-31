{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABCOutput
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Helper for ABC output (pretty printers)
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABCOutput
  ( 
    abcOutput
  ) where

import Payasan.Base.Internal.ABCSyntax
import Payasan.Base.Internal.ABCUtils
import Payasan.Base.Internal.Utils

import Text.PrettyPrint.HughesPJ        -- package: pretty


type CatOp = Doc -> Doc -> Doc

-- Generating output should be stateful so we can insert a 
-- newline every four lines.

type Mon a = Trans () Int a

lineLen :: Mon Int
lineLen = get

resetLineLen :: Mon ()
resetLineLen = put 0

incrLineLen :: Mon ()
incrLineLen = puts (+1)

abcOutput :: ABCPhrase -> Doc
abcOutput ph = evalTrans (oABCPhrase ph) () 0



oABCPhrase :: ABCPhrase -> Mon Doc
oABCPhrase (ABCPhrase [])       = return empty
oABCPhrase (ABCPhrase (x:xs))   = step (oBar x) xs
  where
    step d []     = return d
    step d (b:bs) = do { i <- lineLen
                       ; if i > 4 then resetLineLen else incrLineLen
                       ; let ac = if i > 4 then (d <+> char '|' $+$ oBar b) 
                                           else (d <+> char '|' <+> oBar b)
                       ; step ac bs
                       }


oBar :: Bar -> Doc
oBar (Bar _info cs) = oCtxElementList (<+>) cs

oCtxElementList :: CatOp -> [CtxElement] -> Doc
oCtxElementList op xs = sepList op $ map (oCtxElement op) xs

oCtxElement :: CatOp -> CtxElement -> Doc
oCtxElement _  (Atom e)         = oElement e
oCtxElement _  (Beamed cs)      = oCtxElementList (<>) cs
oCtxElement op (Tuplet spec cs) = tupletSpec spec <> oCtxElementList op cs

oElement :: Element -> Doc
oElement (NoteElem n)         = note n
oElement (Rest d)             = rest d 
oElement (Chord ps d)         = chord ps d 
oElement (Graces xs)          = graceForm $ map note xs
