{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventBar.ShowTabular
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

module Payasan.PSC.Repr.IREventBar.ShowTabular
  ( 

    showTabularIREventBeam
    
  ) where

import Payasan.PSC.Repr.IREventBar.Syntax

import Payasan.PSC.Base.ShowCommon
import Payasan.PSC.Base.ShowTabularUtils

import Payasan.Base.Basis

import Text.PrettyPrint.HughesPJClass                -- package: pretty


showTabularIREventBeam :: LeafOutputEvent pch Seconds anno -> Part pch anno -> Doc
showTabularIREventBeam def ph = concatBars 2 $ oPart def ph


oPart :: LeafOutputEvent pch Seconds anno -> Part pch anno -> [Doc]
oPart def (Part xs)             = map (oSection def) xs

oSection :: LeafOutputEvent pch Seconds anno -> Section pch anno -> Doc
oSection def (Section { section_bars = xs }) = vcat $ map (oBar def) xs

oBar :: LeafOutputEvent pch Seconds anno -> Bar pch anno -> Doc
oBar def (Bar _ xs)             = vcat $ map (oEvent def) xs



oEvent :: LeafOutputEvent pch Seconds anno -> Event pch anno -> Doc
oEvent def (Event ot body)        = ppO ot <++> oEventBody def body
  where
    ppO = pp_onset def


oEventBody :: LeafOutputEvent pch Seconds anno -> EventBody pch anno -> Doc
oEventBody def (Event1 pch drn anno)    = (pp_event def) pch drn anno
oEventBody def (EventGrace pch drn)     = (pp_event_grace def) pch drn
