{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Csound.Output
-- Copyright   :  (c) Stephen Tetley 2016-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
--
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Csound.Output
  ( 

    CsdEventListDoc
  , GenCsdOutput(..)

  , makeCsdEventListDoc

  ) where

import Payasan.PSC.Repr.IREventFlat.Syntax

import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils

import Payasan.Base.Basis

import Text.PrettyPrint.HughesPJ                -- package: pretty


data CsdEventList_
type CsdEventListDoc = TyDoc CsdEventList_


data GenCsdOutput attrs = GenCsdOutput
    { genIStmt          :: Seconds -> Seconds -> attrs -> Doc
    }


makeCsdEventListDoc :: GenCsdOutput attrs
                    -> Part Seconds Seconds attrs 
                    -> CsdEventListDoc
makeCsdEventListDoc def p = TyDoc $ oPart def p


oPart :: GenCsdOutput attrs -> Part Seconds Seconds attrs -> Doc
oPart def (Part { part_sections = ss }) = 
    vsep $ map (renderSection def) ss

    -- columnHeaders (inst_num $ event_body e) specs 


renderSection :: GenCsdOutput attrs -> Section Seconds Seconds attrs -> Doc
renderSection def (Section { section_events = es }) = 
    vsep $ map (renderEvent def) es

renderEvent :: GenCsdOutput attrs -> Event Seconds Seconds attrs -> Doc
renderEvent def (Event { event_onset    = o
                       , event_duration = d
                       , event_attrs    = a}) = (genIStmt def) o d a
