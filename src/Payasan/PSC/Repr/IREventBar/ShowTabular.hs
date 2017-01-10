{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventBar.ShowTabular
-- Copyright   :  (c) Stephen Tetley 2016-2017
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


showTabularIREventBeam :: LeafOutputEvent Seconds pch Seconds anno 
                       -> Part pch anno 
                       -> Doc
showTabularIREventBeam def ph = concatBars 2 $ oPart def ph


oPart :: LeafOutputEvent Seconds pch Seconds anno 
      -> Part pch anno 
      -> [Doc]
oPart def (Part xs)             = map (oSection def) xs

oSection :: LeafOutputEvent Seconds pch Seconds anno
         -> Section pch anno 
         -> Doc
oSection def (Section { section_bars = xs }) = vcat $ map (oBar def) xs

oBar :: LeafOutputEvent Seconds pch Seconds anno 
     -> Bar pch anno -> Doc
oBar def (Bar _ xs)             = vcat $ map (oEvent def) xs



oEvent :: LeafOutputEvent Seconds pch Seconds anno 
       -> Event pch anno 
       -> Doc
oEvent def evt = case evt of
    Event o p d a   -> ppO o <++> (pp_event def) p d a 
    Grace o p d     -> ppO o <++> (pp_event_grace def) p d 
  where
    ppO = pp_onset def

