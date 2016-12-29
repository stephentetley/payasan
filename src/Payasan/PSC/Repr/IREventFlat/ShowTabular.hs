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


showTabularIREventFlat :: LeafOutputEvent ot evt -> Part ot evt -> Doc
showTabularIREventFlat ppl ph = concatBars 2 $ oPart ppl ph


oPart :: LeafOutputEvent ot evt -> Part ot evt -> [Doc]
oPart def (Part xs)             = concat $ map (oSection def) xs

oSection :: LeafOutputEvent ot evt -> Section ot evt -> [Doc]
oSection def (Section { section_events = es })  = map (oEvent def) es

oEvent :: LeafOutputEvent ot evt -> Event ot evt -> Doc
oEvent def (Event ot e)        = ppO ot <++> ppE e
  where
    ppO = pp_onset def
    ppE = pp_event def



