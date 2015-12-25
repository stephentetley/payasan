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



import Payasan.Base.Elementary.Internal.RecalcBars
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
    step n loc | n <= 0    = contentL loc
               | otherwise = step (n-1) $ right loc

drop :: Int -> Phrase pch drn anno -> Phrase pch drn anno
drop i = step i . makeLoc
  where
    step n loc | n <= 0    = contentR loc
               | otherwise = step (n-1) $ right loc



-- | TODO - should last element be untied?
--
takeBars :: Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
takeBars i (Phrase info bs) = Phrase info $ PRE.take i bs

dropBars :: Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
dropBars i (Phrase info bs) = Phrase info $ PRE.drop i bs


-- This has to be an RDuration as Duration is symbolic 
-- and doesn\'t suppoprt addition.
--
takeSize :: forall pch anno.
            RDuration -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
takeSize rd = viaNoteList (\_ xs -> step1 rd xs)
  where
    step1 :: RDuration -> [StdElemNoteGroup2 pch anno]-> [StdElemNoteGroup2 pch anno]
    step1 d _   | d <= 0            = []
    step1 _ []                      = []
    step1 d (Atom e:es)             = 
        let d1 = d - sizeElement e 
        in if d1 < 0 then [] else (Atom e) : step1 d1 es
       

    step1 d (Tuplet spec xs:es)     = 
        let (d1,ys) = step2 d xs 
            spec2   = spec         
        in if null ys then [] else Tuplet spec2 ys : step1 d1 es  -- TODO remake spec


    step2 d []                      = (d,[])
    step2 d (e:es) | d <= 0         = (d,[])
                   | otherwise      = 
        let d1 = d - sizeElement e 
        in if d1 < 0 then (d1,[]) else let (d2,ys) = step2 d1 es in (d2,e:ys)


-- Note - dropSize is really drop-at-least-size as it doesn\'t
-- split too long pivot notes and drops them whole.
--
dropSize :: forall pch anno.
            RDuration -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
dropSize rd = viaNoteList (\_ xs -> step1 rd xs)
  where
    step1 :: RDuration -> [StdElemNoteGroup2 pch anno]-> [StdElemNoteGroup2 pch anno]
    step1 d xs | d <= 0             = xs
    step1 _ []                      = []
    step1 d (Atom e:es)             = 
        let d1 = d - sizeElement e 
        in if d1 <= 0 then es else step1 d1 es


    step1 d (Tuplet spec xs:es)     = case step2 d xs of
        Left d1 -> step1 d1 es
        Right [] -> []
        Right ys -> let spec2 = spec 
                    in Tuplet spec2 ys : es  -- TODO remake spec

    step2 d []                      = Left d
    step2 d (e:es)                  = 
        let d1 = d - sizeElement e
        in if d1 <= 0 then Right es else step2 d1 es
   

