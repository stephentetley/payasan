{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Tabular.OutputMain
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output Main syntax to a Humdrum-like form.
--
-- This is intended debugging and checking purposes, so it is
-- specialized to represent Payasan and is not directly 
-- compatible with Humdrum.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Tabular.OutputMain
  ( 

    mainTabular
    
  ) where

import Payasan.Base.Internal.Tabular.Common
import Payasan.Base.Internal.Tabular.Utils
import Payasan.Base.Internal.MainSyntax
import Payasan.Base.Internal.RewriteMonad


import Text.PrettyPrint.HughesPJClass                -- package: pretty


mainTabular :: LeafOutput pch drn -> Phrase pch drn -> Doc
mainTabular ppl ph = evalRewriteDefault (oPhrase ppl ph) 1


oPhrase :: LeafOutput pch drn -> Phrase pch drn -> OutMon Doc
oPhrase _   (Phrase [])       = return empty
oPhrase ppl (Phrase (x:xs))   = do { d <- oBar ppl x; step d xs }
  where
    step ac []      = return (ac $+$ endSpine <++> endSpine)
    step ac (b:bs)  = do { d <- oBar ppl b; step (ac $+$ d) bs }


oBar :: LeafOutput pch drn -> Bar pch drn -> OutMon Doc
oBar ppl (Bar _info cs) = ($+$) <$> nextBar <*> pure (oNoteGroupList ppl cs)


oNoteGroupList :: LeafOutput pch drn -> [NoteGroup pch drn] -> Doc
oNoteGroupList ppl xs = vcat $ map (oNoteGroup ppl) xs


oNoteGroup :: LeafOutput pch drn -> NoteGroup pch drn -> Doc
oNoteGroup ppl (Atom e)         = oElement ppl e
oNoteGroup ppl (Tuplet _ cs)    = oNoteGroupList ppl cs

oElement :: LeafOutput pch drn -> Element pch drn -> Doc
oElement ppl elt = case elt of
    NoteElem n -> oNote ppl n
    Rest d     -> rest <++> ppD d 
    Chord ps d -> oPitches ppl ps <+> ppD d 
    Graces xs  -> vcat $ map (oNote ppl) xs
  where
    ppD = pp_duration ppl

oNote :: LeafOutput pch drn -> Note pch drn -> Doc
oNote ppl (Note p d)          = ppP p <++> ppD d
  where
    ppP = pp_pitch ppl
    ppD = pp_duration ppl

oPitches :: LeafOutput pch drn -> [pch] -> Doc
oPitches ppl ps = hcat $ punctuate (char ':') $ map ppP ps
  where
    ppP = pp_pitch ppl



