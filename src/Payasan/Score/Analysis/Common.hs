{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Analysis.Common
-- Copyright   :  (c) Stephen Tetley 2015-2016
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

module Payasan.Score.Analysis.Common
  (

    Anchor
  , anchor
  , noAnchor
  , fromAnchor
  
  , Position(..)
  , start_position
  , incPositionBar
  , incPositionIndex

  , Range(..)

  , GrossContour(..)
  , RefinedContour(..)
  , MelodicOutline(..)

  -- * Histogram
  , Histogram
  , empty
  , incr

  ) where



import Data.Data
import qualified Data.Map as MAP

--------------------------------------------------------------------------------
-- Anchors



-- Note - Anchor encapsulates failure (Nothing :: Maybe)
-- (It is expected that...) This allows allows a better
-- API for querying anchors.


newtype Anchor = Anchor { getAnchor :: Maybe Position }


-- | Note - musically it makes sense to count from 1...
--
data Position = Position 
    { position_bar           :: !Int
    , position_index         :: !Int
    }
  deriving (Data,Eq,Ord,Show,Typeable)

start_position :: Position 
start_position = Position { position_bar = 1, position_index = 1}

anchor :: Position -> Anchor
anchor pos = Anchor $ Just $ pos 

noAnchor :: Anchor
noAnchor = Anchor $ Nothing

fromAnchor :: a -> (Int -> Int -> a) -> Anchor -> a
fromAnchor a f = maybe a fPosn . getAnchor
  where
    fPosn (Position b ix) = f b ix

-- Also sets position_index to 1
incPositionBar :: Int -> Position -> Position
incPositionBar n = (\s i -> s { position_bar = i + n, position_index = 1}) <*> position_bar

incPositionIndex :: Int -> Position -> Position
incPositionIndex n = (\s i -> s { position_index = i + n}) <*> position_index


data Range = Range 
    { range_start       :: !Position 
    , range_end         :: !Position 
    }
  deriving (Data,Eq,Ord,Show,Typeable)
                   
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




--------------------------------------------------------------------------------
-- 

-- Histograms?

newtype Histogram a = Histogram { getHistogram :: MAP.Map a Int }
  deriving (Eq,Show)


empty :: Histogram a 
empty = Histogram MAP.empty

incr :: Ord a => a -> Histogram a -> Histogram a
incr k = Histogram . MAP.insertWith fn k 1 . getHistogram
  where
    fn _ i = i+1

-- ideally need a nice printer for Histograms