{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
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

  ) where



import Payasan.Base.Elementary.Internal.Syntax
import Payasan.Base.Elementary.Internal.Zipper


import Payasan.Base.Duration

import Data.Maybe

import Prelude hiding (take, drop)
import qualified Prelude as PRE




--------------------------------------------------------------------------------
--

-- NOTE - would the new Linear view be better?


nth :: Int -> StdElemPhrase2 pch anno -> Maybe (StdElemElement2 pch anno)
nth i = step 0 . viewl . toLinear
  where
    step n ((_,e) :< rest) | n == i    = Just e
                           | otherwise = step (n+1) $ viewl rest
    step _ Empty                       = Nothing



-- nth suggests take and drop
-- drop works easily on Linear, take doesn\'t because 
-- we would have to rebuild from a list (although we can always use 
-- recalcBars). 


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
takeBars :: Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
takeBars i (Phrase info bs) = Phrase info $ PRE.take i bs

dropBars :: Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
dropBars i (Phrase info bs) = Phrase info $ PRE.drop i bs


takeSize :: forall pch anno.
            RDuration -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
takeSize rd = step 0 . makeLoc 
  where
    step sz loc = case atLoc loc of 
                    Just e -> let sz1 = sz + sizeElement e in
                              if sz1 > rd then consumed loc
                                          else step sz1 $ forward loc
                    Nothing -> consumed loc



dropSize :: forall pch anno.
            RDuration -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
dropSize rd = step 0 . makeLoc 
  where
    step sz loc = case atLoc loc of 
                    Just e -> let sz1 = sz + sizeElement e in
                              if sz1 >= rd then remaining loc
                                           else step sz1 $ forward loc
                    Nothing -> remaining loc
