{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Output.Linear.OutputBeam
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output intermediate Beam syntax to a linear form.
--
-- This is intended debugging and checking purposes.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Output.Linear.OutputBeam
  ( 

    beamLinear
    
  ) where

import Payasan.Base.Internal.Output.Common (LeafOutput(..))
import Payasan.Base.Internal.Output.Linear.Utils

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.RewriteMonad


import Text.PrettyPrint.HughesPJClass                -- package: pretty


beamLinear :: LeafOutput pch drn anno -> Phrase pch drn anno -> Doc
beamLinear ppl ph = evalRewriteDefault (oPhrase ppl ph) 1




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
oNoteGroup ppl (Beamed cs)      = oNoteGroupList ppl cs
oNoteGroup ppl (Tuplet _ cs)    = oNoteGroupList ppl cs

oElement :: LeafOutput pch drn anno -> Element pch drn anno -> Doc
oElement ppl elt = case elt of
    NoteElem n _    -> oNote ppl n
    Rest d          -> rest <++> ppD d 
    Chord ps d _    -> oPitches ppl ps <+> ppD d 
    Graces xs       -> vcat $ map (oNote ppl) xs
    Punctuation s   -> text s
  where
    ppD = pp_duration ppl

oNote :: LeafOutput pch drn anno -> Note pch drn -> Doc
oNote ppl (Note p d)          = ppP p <++> ppD d
  where
    ppP = pp_pitch ppl
    ppD = pp_duration ppl

oPitches :: LeafOutput pch drn anno -> [pch] -> Doc
oPitches ppl ps = hcat $ punctuate (char ':') $ map ppP ps
  where
    ppP = pp_pitch ppl


