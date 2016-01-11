{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.Operations
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Phrase operations - often cf Data.List
--
--------------------------------------------------------------------------------

module Payasan.Base.Elementary.Internal.Operations
  (
    nth

  , take
  , drop
  , takeBars
  , dropBars
  , takeSize
  , dropSize

  , takeRange
  , takeWhile
  , dropWhile


  , firstNote
  , lastNote


  ) where


import Payasan.Base.Elementary.Internal.Syntax
import Payasan.Base.Elementary.Internal.Zipper

import Payasan.Base.Internal.AnalysisCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch


import Prelude hiding (take, drop, takeWhile, dropWhile)
import qualified Prelude as PRE


-- Implement Anchors here for the time being...
-- firstNote is easy with Linear view



nth :: Int -> Phrase pch drn anno -> Maybe (Element pch drn anno)
nth i _  | i < 0   = Nothing
nth i ph           = step i $ makeLoc ph
  where
    step n loc | n <= 0    = atLoc loc 
               | otherwise = step (n-1) $ forward loc



firstNote :: Phrase Pitch drn anno -> Anchor
firstNote = step . viewl . toLinear
  where
    step Empty                  = noAnchor
    step ((pos, Note {}) :< _)  = anchor pos
    step ((_,_) :< rest)        = step $ viewl rest


lastNote :: Phrase Pitch drn anno -> Anchor
lastNote = step noAnchor . viewl . toLinear
  where
    step ac Empty                       = ac
    step _  ((pos, Note {}) :< rest)    = step (anchor pos) $ viewl rest
    step ac ((_,_) :< rest)             = step ac $ viewl rest







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


takeWhile :: (Element pch drn anno -> Bool) -> Phrase pch drn anno -> Phrase pch drn anno
takeWhile test = step . makeLoc
  where
    step loc = case atLoc loc of 
                    Just e -> if test e then step $ forward loc 
                                        else consumed loc
                    Nothing -> consumed loc

dropWhile :: (Element pch drn anno -> Bool) -> Phrase pch drn anno -> Phrase pch drn anno
dropWhile test = step . makeLoc
  where
    step loc = case atLoc loc of 
                    Just e -> if test e then step $ forward loc 
                                        else remaining loc
                    Nothing -> remaining loc


{-
-- cf Data.List.words
-- TODO - move from here...
phrases :: Phrase pch drn anno -> [Phrase pch drn anno]
phrases ph@(Phrase {phrase_header = hdr}) = []


-}