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
  , CsdOutputDef

  ) where

import Payasan.PSC.Repr.IREventFlat.Syntax

import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils

import Payasan.Base.Basis

import Text.PrettyPrint.HughesPJ                -- package: pretty


data CsdEventList_
type CsdEventListDoc = TyDoc CsdEventList_


data CsdOutputDef pch anno = CsdOutputDef 
    { iStmtEvent        :: Seconds -> pch -> Seconds -> anno -> Doc
    , iStmtEventGrace   :: Seconds -> pch -> Seconds -> Doc
    }


makeCsdEventListDoc :: CsdOutputDef pch anno -> Part Seconds pch Seconds anno -> CsdEventListDoc
makeCsdEventListDoc specs p = TyDoc empty


oPart :: CsdOutputDef pch anno -> Part Seconds pch Seconds anno -> Doc
oPart def (Part { part_sections = ss }) = 
    vsep $ map (renderSection def) ss
    -- columnHeaders (inst_num $ event_body e) specs 


renderSection :: CsdOutputDef pch anno -> Section Seconds pch Seconds anno -> Doc
renderSection specs (Section { section_events = es}) = empty
