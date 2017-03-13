{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.ABC.IRSimpleDoc
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Intermediate representation for easy printing.
--
--------------------------------------------------------------------------------

module Payasan.PSC.ABC.IRSimpleDoc
  ( 
    Part(..) 
  , Section(..)
  , Bar(..)

  ) where


import Text.PrettyPrint.HughesPJ hiding ( Mode )       -- package: pretty


-- Polymorphic @a@ is a phantom.

data Part a = Part
    { part_sections :: [Section a] 
    }
  deriving (Show)


data Section a = Section 
    { section_name      :: !String
    , section_header    :: Maybe Doc
    , section_bars      :: [Bar a]
    }
  deriving (Show)


data Bar a = Bar 
    { bar_content :: Doc
    }
  deriving (Show)

