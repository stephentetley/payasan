{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Contour
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Datatypes for melodic Contours.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Contour
  (

    GrossContour(..)
  , RefinedContour(..)


  ) where



import Data.Data

-- TODO...
--
-- This module is too small to be independent, but we
-- don\'t want to complicate Internal.CommonSyntax with 
-- datatypes thta are for /analysis/ rather than construction
-- (and representation).
--
-- When we add more analyses this module should be extended and renamed.
--

--------------------------------------------------------------------------------
-- Contours



data GrossContour = DOWN | GROSS_SAME | UP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


-- | Leap is a interval distance > 2 ( a second)
--
data RefinedContour = LEAP_DOWN | STEP_DOWN | REFINED_SAME | STEP_UP | LEAP_UP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

