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
  , ColumnFormats(..)

  , Value(..)  -- ideally shouldn't expose VEllipsis to users
  , istmtDoc

  , makeCsdEventListDoc

  ) where


import Payasan.PSC.Csound.Utils

import Payasan.PSC.Repr.IREventFlat.Syntax

import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils

import Payasan.Base.Basis

import Text.PrettyPrint.HughesPJ                -- package: pretty


data CsdEventList_
type CsdEventListDoc = TyDoc CsdEventList_


-- It would likely be better to generate a list of 'Values' 
-- then we can eleviate from the user some of the detail of 
-- printing them.
--
data GenCsdOutput attrs = GenCsdOutput
    { instr_number      :: Int
    , column_specs      :: ColumnFormats
    , genAttrValues     :: attrs -> [Value]
    }


data ColumnFormats = ColumnFormats
    { inst_colwidth     :: Int
    , time_colformat    :: (Int,Int)
    , other_widths      :: [Int]
    }
  deriving (Eq,Show)

type Precision = Int


-- | Value with booleans - slightly higher level than Csound 
-- which has no booleans.
-- 
-- Floats are twinned with the precision to print them at.
--
data Value = VStr       !String
           | VInt       !Int
           | VFloat     Precision   !Decimal
           | VBool      !Bool
           | VEllipsis
  deriving (Eq,Show)


renderValue :: Int -> Value -> Doc
renderValue w (VStr ss)         = stringp w ss
renderValue w (VInt i)          = intp w i
renderValue w (VFloat p d)      = decimalp (w,p) d
renderValue w (VBool b)         = boolp w b
renderValue w (VEllipsis)       = textp w "."

istmtDoc :: ColumnFormats -> Int -> Seconds -> Seconds -> [Value] -> Doc
istmtDoc (ColumnFormats { inst_colwidth   = instw
                        , time_colformat  = timefmt
                        , other_widths    = widths }) inst ot drn vals = 
    instcol <+> decimalp timefmt ot <+> decimalp timefmt drn <+> go widths vals
  where
    instcol             = textp instw ('i':show inst)
    go (w:ws) (v:vs)    = renderValue w v <+> go ws vs
    go []     (v:vs)    = renderValue 8 v <+> go [] vs
    go _      []        = empty


makeCsdEventListDoc :: GenCsdOutput attrs
                    -> Part Seconds Seconds attrs 
                    -> CsdEventListDoc
makeCsdEventListDoc def p = TyDoc $ oPart def p


oPart :: GenCsdOutput attrs -> Part Seconds Seconds attrs -> Doc
oPart def (Part { part_sections = ss }) = 
    vsep $ map (renderSection def) ss


renderSection :: GenCsdOutput attrs -> Section Seconds Seconds attrs -> Doc
renderSection def (Section { section_events = es }) = 
    vsep $ map (renderEvent def) es

renderEvent :: GenCsdOutput attrs -> Event Seconds Seconds attrs -> Doc
renderEvent def (Event { event_onset    = o
                       , event_duration = d
                       , event_attrs    = a}) = 
    let values  = genAttrValues def a
    in istmtDoc (column_specs def) (instr_number def) o d values
