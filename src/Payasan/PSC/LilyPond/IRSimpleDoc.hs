{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.LilyPond.IRSimpleDoc
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

module Payasan.PSC.LilyPond.IRSimpleDoc
  ( 
    Part(..) 
  , ContextDoc(..)
  , Section(..)
  , Bar(..)

  ) where


import Text.PrettyPrint.HughesPJ ( Doc )        -- package: pretty


-- Polymorphic @a@ is a phantom.

data Part a = Part
    { part_sections :: [Section a] 
    }
  deriving (Show)



-- | BlockDoc is a Doc transformer, this potentially allows 
-- prefix-suffix blocks as well as just prefix (which is all 
-- ABC needs).
--
newtype ContextDoc = ContextDoc { getContext :: Doc -> Doc }

instance Show ContextDoc where
  show _ = "<#Context>"


instance Monoid ContextDoc where
  mempty = ContextDoc id
  ContextDoc f `mappend` ContextDoc g = ContextDoc $ \d -> f (g d)



data Section a = Section 
    { section_name      :: !String
    , section_context   :: ContextDoc
    , section_bars      :: [Bar a]
    }
  deriving (Show)


data Bar a = Bar 
    { bar_content :: Doc
    }
  deriving (Show)



