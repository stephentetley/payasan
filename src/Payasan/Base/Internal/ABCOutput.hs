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

import Text.PrettyPrint.HughesPJ        -- package: pretty


type CatOp = Doc -> Doc -> Doc

abcOutput :: ABCPhrase -> Doc
abcOutput = oABCPhrase

oABCPhrase :: ABCPhrase -> Doc
oABCPhrase (ABCPhrase [])       = empty
oABCPhrase (ABCPhrase (x:xs))   = step (oBar x) xs
  where
    step d (b:bs) = let ac = d <+> char '|' <+> oBar b in step ac bs
    step d []     = d


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
