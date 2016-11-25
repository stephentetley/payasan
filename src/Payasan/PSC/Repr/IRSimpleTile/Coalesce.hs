{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IRSimpleTile.Coalesce
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Join ties, time steal for graces, 
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IRSimpleTile.Coalesce
  ( 
    joinTies
  
  ) where


import Payasan.PSC.Repr.IRSimpleTile.Syntax



joinTies :: Part pch anno -> Part pch anno
joinTies (Part { part_sections = ss}) = Part { part_sections = map sectionT ss }

-- Note - ties can span bars (in input) but cannot span sections

-- TODO
sectionT :: Section pch anno -> Section pch anno
sectionT = id


-- If we want we could translate graces to notes, but conversion 
-- to IREventBeam is so simple it doesn't make much difference.

