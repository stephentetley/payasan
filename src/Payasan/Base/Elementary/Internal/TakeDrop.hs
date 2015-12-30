{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.TakeDrop
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Take and Drop operations - experimental so in a separate 
-- module for the time being.
--
--------------------------------------------------------------------------------

module Payasan.Base.Elementary.Internal.TakeDrop
  (
    nth
  , take
  , drop
  , takeBars
  , dropBars
  , takeSize
  , dropSize

  , takeRange

  ) where



import Payasan.Base.Elementary.Internal.Syntax
import Payasan.Base.Elementary.Internal.Zipper


import Payasan.Base.Internal.AnalysisCommon
import Payasan.Base.Duration

import Data.Maybe

import Prelude hiding (take, drop)
import qualified Prelude as PRE




--------------------------------------------------------------------------------
--

-- NOTE - Zipper seems to beat Linear for clarity.




nth :: Int -> Phrase pch drn anno -> Maybe (Element pch drn anno)
nth i _  | i < 0   = Nothing
nth i ph           = step i $ makeLoc ph
  where
    step n loc | n <= 0    = atLoc loc 
               | otherwise = step (n-1) $ forward loc



-- Design Note - 23.12.2015 
-- Tuplets don\'t naturally contract (remove notes) or extend 
-- (add notes). 
-- 
-- There are two possible ways of extending a tuplet:
--
-- i. Changing the "base"
-- 3 notes in time of 2 becomes 4 notes in the time of 3
--
-- ii. Keeping the base constant
-- 3 notes in time of 2 becomes 4 notes in the time of 2
-- 
-- Both ways alter all the durations in the tuplet.
--
--




take :: Int -> Phrase pch drn anno -> Phrase pch drn anno
take i = step i . makeLoc
  where
    step n loc | n <= 0    = consumed loc
               | otherwise = step (n-1) $ forward loc


drop :: Int -> Phrase pch drn anno -> Phrase pch drn anno
drop i = step i . makeLoc
  where
    step n loc | n <= 0    = remaining loc
               | otherwise = step (n-1) $ forward loc



-- | TODO - should last element be untied?
--
takeBars :: Int -> Phrase pch drn anno -> Phrase pch drn anno
takeBars i (Phrase info bs) = Phrase info $ PRE.take i bs

dropBars :: Int -> Phrase pch drn anno -> Phrase pch drn anno
dropBars i (Phrase info bs) = Phrase info $ PRE.drop i bs


takeSize :: RDuration -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
takeSize rd = step 0 . makeLoc 
  where
    step sz loc = case atLoc loc of 
                    Just e -> let sz1 = sz + sizeElement e in
                              if sz1 > rd then consumed loc
                                          else step sz1 $ forward loc
                    Nothing -> consumed loc



dropSize :: RDuration -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
dropSize rd = step 0 . makeLoc 
  where
    step sz loc = case atLoc loc of 
                    Just e -> let sz1 = sz + sizeElement e in
                              if sz1 >= rd then remaining loc
                                           else step sz1 $ forward loc
                    Nothing -> remaining loc



takeRange :: Range -> Phrase pch drn anno -> Phrase pch drn anno
takeRange (Range {range_start = p1, range_end = p2}) ph = 
    let ph1 = consumed $ gotoPosition p2 $ makeLoc ph
        ph2 = remaining $ gotoPosition p1 $ makeLoc ph1
    in ph2
