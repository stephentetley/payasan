{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.External.ShowTabular
-- Copyright   :  (c) Stephen Tetley 2015-2016
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

module Payasan.PSC.Repr.External.ShowTabular
  ( 

    mainTabular
    
  ) where

import Payasan.PSC.Repr.External.Syntax

import Payasan.PSC.Base.ShowCommon
import Payasan.PSC.Base.ShowTabularUtils


import Text.PrettyPrint.HughesPJClass                -- package: pretty


mainTabular :: LeafOutputNote pch drn anno -> Part pch drn anno -> Doc
mainTabular ppl ph = concatBars 2 $ oPart ppl ph


oPart :: LeafOutputNote pch drn anno -> Part pch drn anno -> [Doc]
oPart ppl (Part xs)             = map (oBar ppl) xs


oBar :: LeafOutputNote pch drn anno -> Bar pch drn anno -> Doc
oBar ppl (Bar _ cs)             = oNoteGroupList ppl cs


oNoteGroupList :: LeafOutputNote pch drn anno -> [NoteGroup pch drn anno] -> Doc
oNoteGroupList ppl xs           = vcat $ map (oNoteGroup ppl) xs


oNoteGroup :: LeafOutputNote pch drn anno -> NoteGroup pch drn anno -> Doc
oNoteGroup ppl (Atom e)         = oElement ppl e
oNoteGroup ppl (Tuplet _ cs)    = oNoteGroupList ppl cs

oElement :: LeafOutputNote pch drn anno -> Element pch drn anno -> Doc
oElement ppl elt = case elt of
    NoteElem n _ _      -> oNote ppl n
    Rest d              -> rest <++> ppD d 
    Spacer d            -> spacer <++> ppD d 
    Skip d              -> skip <++> ppD d 
    Chord ps d _ _      -> oPitches ppl ps <+> ppD d 
    Graces xs           -> vcat $ map (oNote ppl) xs
    Punctuation {}      -> empty
  where
    ppD = pp_duration ppl

oNote :: LeafOutputNote pch drn anno -> Note pch drn -> Doc
oNote ppl (Note p d)          = ppP p <++> ppD d
  where
    ppP = pp_pitch ppl
    ppD = pp_duration ppl

oPitches :: LeafOutputNote pch drn anno -> [pch] -> Doc
oPitches ppl ps = hcat $ punctuate (char ':') $ map ppP ps
  where
    ppP = pp_pitch ppl



