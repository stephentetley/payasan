{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.TabularOutput
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output Elementary syntax to a Humdrum-like form.
--
-- This is intended debugging and checking purposes, so it is
-- specialized to represent Payasan and is not directly 
-- compatible with Humdrum.
--
--------------------------------------------------------------------------------

module Payasan.Base.Elementary.Internal.TabularOutput
  ( 

    elemTabular
    
  ) where

import Payasan.Base.Elementary.Internal.Syntax

import Payasan.Base.Internal.Output.Common
import Payasan.Base.Internal.Output.Tabular.Utils


import Text.PrettyPrint.HughesPJClass                -- package: pretty


elemTabular :: LeafOutput pch drn anno -> Phrase pch drn anno -> Doc
elemTabular ppl ph = concatBars 2 $ oPhrase ppl ph




oPhrase :: LeafOutput pch drn anno -> Phrase pch drn anno -> [Doc]
oPhrase ppl  (Phrase _  xs)     = map (oBar ppl) xs


oBar :: LeafOutput pch drn anno -> Bar pch drn anno -> Doc
oBar ppl (Bar cs) = oNoteGroupList ppl cs


oNoteGroupList :: LeafOutput pch drn anno -> [NoteGroup pch drn anno] -> Doc
oNoteGroupList ppl xs = vcat $ map (oNoteGroup ppl) xs


oNoteGroup :: LeafOutput pch drn anno -> NoteGroup pch drn anno -> Doc
oNoteGroup ppl (Atom e)         = oElement ppl e
oNoteGroup ppl (Tuplet _ cs)    = vcat $ map (oElement ppl) cs

oElement :: LeafOutput pch drn anno -> Element pch drn anno -> Doc
oElement ppl elt = case elt of
    Note p d _ _    -> ppP p <++> ppD d
    Rest d          -> rest  <++> ppD d 
    Spacer d        -> spacer <++> ppD d 
    Skip d          -> skip  <++> ppD d 
    Punctuation {}  -> empty
  where
    ppP = pp_pitch ppl
    ppD = pp_duration ppl




