{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventBeam.ShowTabular
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output IREventBeam syntax to a Humdrum-like event list form.
--
-- This is intended debugging and checking purposes, so it is
-- specialized to represent Payasan and is not directly 
-- compatible with Humdrum.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IREventBeam.ShowTabular
  ( 

    showTabularIREventBeam
    
  ) where

import Payasan.PSC.Repr.IREventBeam.Syntax

import Payasan.PSC.Base.ShowCommon
import Payasan.PSC.Base.ShowTabularUtils


import Text.PrettyPrint.HughesPJClass                -- package: pretty


showTabularIREventBeam :: LeafOutputEvent ot evt -> Part ot evt -> Doc
showTabularIREventBeam ppl ph = concatBars 2 $ oPart ppl ph


oPart :: LeafOutputEvent ot evt -> Part ot evt -> [Doc]
oPart ppl (Part xs)             = map (oSection ppl) xs

oSection :: LeafOutputEvent ot evt -> Section ot evt -> Doc
oSection ppl (Section { section_bars = xs }) = vcat $ map (oBar ppl) xs

oBar :: LeafOutputEvent ot evt -> Bar ot evt -> Doc
oBar ppl (Bar _ xs)             = vcat $ map (oEvent ppl) xs



oEvent :: LeafOutputEvent ot evt -> Event ot evt -> Doc
oEvent ppl (Event ot e)        = ppO ot <++> ppE e
  where
    ppO = pp_onset ppl
    ppE = pp_event ppl



