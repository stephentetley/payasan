{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.TabularOutput
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output Mono syntax to a Humdrum-like form.
--
-- This is intended debugging and checking purposes, so it is
-- specialized to represent Payasan and is not directly 
-- compatible with Humdrum.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.TabularOutput
  ( 

    monoTabular
    
  ) where

import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.Tabular.Common
import Payasan.Base.Internal.Tabular.Utils
import Payasan.Base.Internal.RewriteMonad


import Text.PrettyPrint.HughesPJClass                -- package: pretty


monoTabular :: LeafOutput pch drn anno -> Phrase pch drn anno -> Doc
monoTabular ppl ph = evalRewriteDefault (oPhrase ppl ph) 1




oPhrase :: LeafOutput pch drn anno -> Phrase pch drn anno -> OutMon Doc
oPhrase _   (Phrase [])       = return empty
oPhrase ppl (Phrase (x:xs))   = do { d <- oBar ppl x; step d xs }
  where
    step ac []      = return (ac $+$ endSpine <++> endSpine)
    step ac (b:bs)  = do { d <- oBar ppl b; step (ac $+$ d) bs }


oBar :: LeafOutput pch drn anno -> Bar pch drn anno -> OutMon Doc
oBar ppl (Bar _info cs) = ($+$) <$> nextBar <*> pure (oNoteGroupList ppl cs)


oNoteGroupList :: LeafOutput pch drn anno -> [NoteGroup pch drn anno] -> Doc
oNoteGroupList ppl xs = vcat $ map (oNoteGroup ppl) xs


oNoteGroup :: LeafOutput pch drn anno -> NoteGroup pch drn anno -> Doc
oNoteGroup ppl (Atom e)         = oElement ppl e
oNoteGroup ppl (Tuplet _ cs)    = oNoteGroupList ppl cs

oElement :: LeafOutput pch drn anno -> Element pch drn anno -> Doc
oElement ppl elt = case elt of
    Note p d a     -> ppP p <++> ppD d
    Rest d          -> rest <++> ppD d 
  where
    ppP = pp_pitch ppl
    ppD = pp_duration ppl



oPitches :: LeafOutput pch drn anno -> [pch] -> Doc
oPitches ppl ps = hcat $ punctuate (char ':') $ map ppP ps
  where
    ppP = pp_pitch ppl



