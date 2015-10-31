{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Output.Linear.OutputMain
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output Main syntax to a linear form.
--
-- This is intended debugging and checking purposes.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Output.Linear.OutputMain
  ( 

    mainLinear
    
  ) where

import Payasan.Base.Internal.Output.Common
import Payasan.Base.Internal.Output.Linear.Utils

import Payasan.Base.Internal.MainSyntax


import Text.PrettyPrint.HughesPJClass                -- package: pretty


mainLinear :: LeafOutput pch drn anno -> Phrase pch drn anno -> Doc
mainLinear ppl ph = concatBars $ oPhrase ppl ph


oPhrase :: LeafOutput pch drn anno -> Phrase pch drn anno -> [Doc]
oPhrase ppl (Phrase xs)         = map (oBar ppl) xs


oBar :: LeafOutput pch drn anno -> Bar pch drn anno -> Doc
oBar ppl (Bar _info cs)         =  oNoteGroupList ppl cs


oNoteGroupList :: LeafOutput pch drn anno -> [NoteGroup pch drn anno] -> Doc
oNoteGroupList ppl xs = vcat $ map (oNoteGroup ppl) xs


oNoteGroup :: LeafOutput pch drn anno -> NoteGroup pch drn anno -> Doc
oNoteGroup ppl (Atom e)         = oElement ppl e
oNoteGroup ppl (Tuplet _ cs)    = oNoteGroupList ppl cs

oElement :: LeafOutput pch drn anno -> Element pch drn anno -> Doc
oElement ppl elt = case elt of
    NoteElem n _ _ _    -> oNote ppl n
    Rest d              -> rest <> ppD d 
    Spacer d            -> spacer <> ppD d 
    Skip d              -> skip <> ppD d 
    Chord ps d _ _ _    -> oPitches ppl ps <+> ppD d 
    Graces xs           -> vcat $ map (oNote ppl) xs
    Punctuation s       -> text s
  where
    ppD = pp_duration ppl

oNote :: LeafOutput pch drn anno -> Note pch drn -> Doc
oNote ppl (Note p d)          = ppP p <> ppD d
  where
    ppP = pp_pitch ppl
    ppD = pp_duration ppl

oPitches :: LeafOutput pch drn anno -> [pch] -> Doc
oPitches ppl ps = hcat $ punctuate (char ':') $ map ppP ps
  where
    ppP = pp_pitch ppl



