{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.AnalysisCommon
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Datatypes and common code for analyses and introspection.
-- 
-- Contours, anchors, etc.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.AnalysisCommon
  (

    Anchor
  , anchor
  , noAnchor
  , fromAnchor
  
  , Position(..)
  , incPositionBar
  , incPositionIndex

  , GrossContour(..)
  , RefinedContour(..)
  , MelodicOutline(..)

  ) where



import Data.Data


--------------------------------------------------------------------------------
-- Anchors



-- Note - Anchor encapsulates failure (Nothing :: Maybe)
-- (It is expected that...) This allows allows a better
-- API for querying anchors.


newtype Anchor = Anchor { getAnchor :: Maybe Position }

data Position = Position 
    { position_bar           :: !Int
    , position_index         :: !Int
    }
  deriving (Data,Eq,Ord,Show,Typeable)


anchor :: Position -> Anchor
anchor pos = Anchor $ Just $ pos 

noAnchor :: Anchor
noAnchor = Anchor $ Nothing

fromAnchor :: a -> (Int -> Int -> a) -> Anchor -> a
fromAnchor a f = maybe a fPosn . getAnchor
  where
    fPosn (Position b ix) = f b ix


incPositionBar :: Int -> Position -> Position
incPositionBar n = (\s i -> s { position_bar = i + n}) <*> position_bar

incPositionIndex :: Int -> Position -> Position
incPositionIndex n = (\s i -> s { position_index = i + n}) <*> position_index
                   
--------------------------------------------------------------------------------
-- Contours


data GrossContour = DOWN | GROSS_SAME | UP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


-- | Leap is a interval distance > 2 ( a second)
--
data RefinedContour = LEAP_DOWN | STEP_DOWN | REFINED_SAME | STEP_UP | LEAP_UP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


-- TODO - what about arch?
--
data MelodicOutline = ASCENDING | DESCENDING | STATIONARY
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


