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
-- Operations on Parts - often cf Data.List
--
--------------------------------------------------------------------------------

module Payasan.Base.Elementary.Internal.Operations
  (

    isNote
  , isRestlike
  , isPunctuation


  , null
  , nth

  , firstNote
  , lastNote

  , take
  , drop
  , takeBars
  , dropBars
  , takeSize
  , dropSize

  , extractRange

  , takeWhile
  , dropWhile

  , span
  , break

  , phrases

  ) where


import Payasan.Base.Elementary.Internal.Syntax
import Payasan.Base.Elementary.Internal.Zipper

import Payasan.Base.Internal.AnalysisCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch


import Prelude hiding (null, take, drop, takeWhile, dropWhile, span, break)
import qualified Prelude as PRE



-- cf. Data.Char character classes...

isNote :: Element pdh drn anno -> Bool
isNote (Note {})                = True
isNote _                        = False

isRestlike :: Element pdh drn anno -> Bool
isRestlike (Note {})            = False
isRestlike (Rest {})            = True
isRestlike (Spacer {})          = True
isRestlike (Skip {})            = True
isRestlike (Punctuation {})     = False

isPunctuation :: Element pdh drn anno -> Bool
isPunctuation (Punctuation {})  = True
isPunctuation _                 = False



--------------------------------------------------------------------------------
-- ops on phrases


null :: Part pch drn anno -> Bool
null (Part { part_bars = xs }) = PRE.null xs


nth :: Int -> Part pch drn anno -> Maybe (Element pch drn anno)
nth i _  | i < 0   = Nothing
nth i ph           = step i $ makeLoc ph
  where
    step n loc | n <= 0    = atLoc loc 
               | otherwise = step (n-1) $ forward loc



firstNote :: Part Pitch drn anno -> Anchor
firstNote = step . viewl . toLinear
  where
    step Empty                  = noAnchor
    step ((pos, Note {}) :< _)  = anchor pos
    step ((_,_) :< rest)        = step $ viewl rest


lastNote :: Part Pitch drn anno -> Anchor
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




take :: Int -> Part pch drn anno -> Part pch drn anno
take i = step i . makeLoc
  where
    step n loc | n <= 0    = consumed loc
               | otherwise = step (n-1) $ forward loc


drop :: Int -> Part pch drn anno -> Part pch drn anno
drop i = step i . makeLoc
  where
    step n loc | n <= 0    = remaining loc
               | otherwise = step (n-1) $ forward loc



-- | TODO - should last element be untied?
--
takeBars :: Int -> Part pch drn anno -> Part pch drn anno
takeBars i (Part info bs) = Part info $ PRE.take i bs

dropBars :: Int -> Part pch drn anno -> Part pch drn anno
dropBars i (Part info bs) = Part info $ PRE.drop i bs


takeSize :: RDuration -> StdElemPart2 pch anno -> StdElemPart2 pch anno
takeSize rd = step 0 . makeLoc 
  where
    step sz loc = case atLoc loc of 
                    Just e -> let sz1 = sz + sizeElement e in
                              if sz1 > rd then consumed loc
                                          else step sz1 $ forward loc
                    Nothing -> consumed loc



dropSize :: RDuration -> StdElemPart2 pch anno -> StdElemPart2 pch anno
dropSize rd = step 0 . makeLoc 
  where
    step sz loc = case atLoc loc of 
                    Just e -> let sz1 = sz + sizeElement e in
                              if sz1 >= rd then remaining loc
                                           else step sz1 $ forward loc
                    Nothing -> remaining loc



extractRange :: Range -> Part pch drn anno -> Part pch drn anno
extractRange (Range {range_start = p1, range_end = p2}) ph = 
    let ph1 = consumed $ gotoPosition p2 $ makeLoc ph
        ph2 = remaining $ gotoPosition p1 $ makeLoc ph1
    in ph2


takeWhile :: (Element pch drn anno -> Bool) -> Part pch drn anno -> Part pch drn anno
takeWhile test = step . makeLoc
  where
    step loc = case atLoc loc of 
                    Just e -> if test e then step $ forward loc 
                                        else consumed loc
                    Nothing -> consumed loc

dropWhile :: (Element pch drn anno -> Bool) -> Part pch drn anno -> Part pch drn anno
dropWhile test = step . makeLoc
  where
    step loc = case atLoc loc of 
                    Just e -> if test e then step $ forward loc 
                                        else remaining loc
                    Nothing -> remaining loc



span :: (Element pch drn anno -> Bool) 
     -> Part pch drn anno 
     -> (Part pch drn anno, Part pch drn anno)
span test = step . makeLoc
  where
    step loc = case atLoc loc of 
                    Just e -> if test e then step $ forward loc 
                                        else (consumed loc, remaining loc)
                    Nothing -> (consumed loc, remaining loc)




break :: (Element pch drn anno -> Bool) 
      -> Part pch drn anno 
      -> (Part pch drn anno, Part pch drn anno)
break test = span (not . test)



-- cf Data.List.words
-- TODO - move from here...
phrases :: Part pch drn anno -> [Part pch drn anno]
phrases pt = let ans = dropWhile isRestlike pt in 
             if null ans then []
                         else let (b,bs) = break isRestlike ans
                              in b : phrases bs
   



