{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.Output
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output ABC.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABC.Output
  ( 
    abcOutput
  ) where

import Payasan.Base.Internal.ABC.Syntax
import Payasan.Base.Internal.ABC.Utils
import Payasan.Base.Internal.RewriteMonad

import Text.PrettyPrint.HughesPJ hiding ( Mode )       -- package: pretty


type CatOp = Doc -> Doc -> Doc

-- Generating output should be stateful so we can insert a 
-- newline every four lines.

type Mon a = Rewrite Int a

lineLen :: Mon Int
lineLen = get

resetLineLen :: Mon ()
resetLineLen = put 0

incrLineLen :: Mon ()
incrLineLen = puts (+1)

abcOutput :: GlobalRenderInfo -> ABCPhrase -> Doc
abcOutput info ph = header $+$ body
   where
     header = oHeader info (maybe default_local_info id $ firstRenderInfo ph)
     body   = evalRewriteDefault (oABCPhrase ph) 0


oHeader :: GlobalRenderInfo -> LocalRenderInfo -> Doc
oHeader globals locals = 
        field 'X' (int 1)
    $+$ field 'T' (text   $ global_title globals)
    $+$ field 'K' (key    $ local_key_sig locals)
    $+$ field 'M' (meter  $ local_time_sig locals)
    $+$ field 'L' (unitNoteLength $ local_unit_note_len locals)


oABCPhrase :: ABCPhrase -> Mon Doc
oABCPhrase (Phrase [])          = return empty
oABCPhrase (Phrase (x:xs))      = step (oBar x) xs
  where
    step d []     = return $ d <+> text "|]"
    step d (b:bs) = do { i <- lineLen
                       ; if i > 4 then resetLineLen else incrLineLen
                       ; let ac = if i > 4 then (d <+> char '|' $+$ oBar b) 
                                           else (d <+> char '|' <+> oBar b)
                       ; step ac bs
                       }


oBar :: ABCBar -> Doc
oBar (Bar _info cs) = oCtxElementList (<+>) cs

oCtxElementList :: CatOp -> [ABCCtxElement] -> Doc
oCtxElementList op xs = sepList op $ map (oCtxElement op) xs

oCtxElement :: CatOp -> ABCCtxElement -> Doc
oCtxElement _  (Atom e)         = oElement e
oCtxElement _  (Beamed cs)      = oCtxElementList (<>) cs
oCtxElement op (Tuplet spec cs) = tupletSpec spec <> oCtxElementList op cs

oElement :: ABCElement -> Doc
oElement (NoteElem n)         = note n
oElement (Rest d)             = rest d 
oElement (Chord ps d)         = chord ps d 
oElement (Graces xs)          = graceForm $ map note xs
