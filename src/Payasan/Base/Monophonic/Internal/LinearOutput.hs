{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.LinearOutput
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output Mono syntax to a linear form.
--
-- This is intended debugging and checking purposes.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.LinearOutput
  ( 

    monoLinear
    
  ) where

import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.Output.Common
import Payasan.Base.Internal.Output.Linear.Utils


import Text.PrettyPrint.HughesPJClass                -- package: pretty


-- NOTE - if we use Note-Rest as Maybe+duration we need
-- a larger customiztion than LeafOuput.

monoLinear :: LeafOutput pch drn anno -> Phrase pch drn anno -> Doc
monoLinear ppl ph = concatBars $ oPhrase ppl ph




oPhrase :: LeafOutput pch drn anno -> Phrase pch drn anno -> [Doc]
oPhrase ppl (Phrase _ xs)       = map (oBar ppl) xs


oBar :: LeafOutput pch drn anno -> Bar pch drn anno -> Doc
oBar ppl (Bar cs)               = oNoteGroupList ppl cs


oNoteGroupList :: LeafOutput pch drn anno -> [NoteGroup pch drn anno] -> Doc
oNoteGroupList ppl xs           = hsep $ map (oNoteGroup ppl) xs


oNoteGroup :: LeafOutput pch drn anno -> NoteGroup pch drn anno -> Doc
oNoteGroup ppl (Atom e)         = oElement ppl e
oNoteGroup ppl (Tuplet _ cs)    = oNoteGroupList ppl cs

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



