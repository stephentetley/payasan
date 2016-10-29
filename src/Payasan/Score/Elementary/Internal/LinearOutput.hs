{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.LinearOutput
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output Elementary syntax to a linear form.
--
-- This is intended debugging and checking purposes.
--
--------------------------------------------------------------------------------

module Payasan.Score.Elementary.Internal.LinearOutput
  ( 

    elemLinear
    
  ) where

import Payasan.Score.Elementary.Internal.Syntax

import Payasan.PSC.Backend.Output.Common
import Payasan.PSC.Backend.Output.Linear.Utils


import Text.PrettyPrint.HughesPJClass                -- package: pretty


-- NOTE - if we use Note-Rest as Maybe+duration we need
-- a larger customiztion than LeafOuput.

elemLinear :: LeafOutput pch drn anno -> Part pch drn anno -> Doc
elemLinear ppl ph = concatBars $ oPart ppl ph




oPart :: LeafOutput pch drn anno -> Part pch drn anno -> [Doc]
oPart ppl (Part _ xs)           = map (oBar ppl) xs


oBar :: LeafOutput pch drn anno -> Bar pch drn anno -> Doc
oBar ppl (Bar cs)               = oNoteGroupList ppl cs


oNoteGroupList :: LeafOutput pch drn anno -> [NoteGroup pch drn anno] -> Doc
oNoteGroupList ppl xs           = hsep $ map (oNoteGroup ppl) xs


oNoteGroup :: LeafOutput pch drn anno -> NoteGroup pch drn anno -> Doc
oNoteGroup ppl (Atom e)         = oElement ppl e
oNoteGroup ppl (Tuplet _ es)    = hsep $ map (oElement ppl) es

oElement :: LeafOutput pch drn anno -> Element pch drn anno -> Doc
oElement ppl elt = case elt of
    Note p d _ _        -> ppP p <> char ':' <> ppD d
    Rest d              -> rest  <> ppD d 
    Spacer d            -> spacer <> ppD d 
    Skip d              -> skip  <> ppD d 
    Punctuation s       -> text s
  where
    ppP = pp_pitch ppl
    ppD = pp_duration ppl



