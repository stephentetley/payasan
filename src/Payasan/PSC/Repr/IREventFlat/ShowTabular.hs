{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventFlat.ShowTabular
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output IREventFlat syntax to a Humdrum-like event list form.
--
-- This is intended debugging and checking purposes, so it is
-- specialized to represent Payasan and is not directly 
-- compatible with Humdrum.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IREventFlat.ShowTabular
  ( 

    showTabularIREventFlat
    
  ) where

import Payasan.PSC.Repr.IREventFlat.Syntax

import Payasan.PSC.Base.ShowCommon
import Payasan.PSC.Base.ShowTabularUtils

import Text.PrettyPrint.HughesPJClass                -- package: pretty


showTabularIREventFlat :: LeafOutputEvent pch time anno -> Part pch time anno -> Doc
showTabularIREventFlat def ph = concatBars 2 $ oPart def ph


oPart :: LeafOutputEvent pch time anno -> Part pch time anno -> [Doc]
oPart def (Part xs)             = concat $ map (oSection def) xs

oSection :: LeafOutputEvent pch time anno -> Section pch time anno -> [Doc]
oSection def (Section { section_events = es })  = map (oEvent def) es

oEvent :: LeafOutputEvent pch time anno -> Event pch time anno -> Doc
oEvent def (Event ot body)        = ppO ot <++> oEventBody def body
  where
    ppO = pp_onset def


oEventBody :: LeafOutputEvent pch time anno -> EventBody pch time anno -> Doc
oEventBody def (Event1 pch drn anno)    = (pp_event def) pch drn anno
oEventBody def (EventGrace pch drn)     = (pp_event_grace def) pch drn



