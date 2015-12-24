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

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration

import Data.Foldable (foldlM)
import Data.Maybe

import Prelude hiding (take, drop)
import qualified Prelude as PRE

type Mon st a = Rewrite st a



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




-- | Tuplet splitting is not properly implemented yet as it 
-- should modify the spec
--
-- TODO - it will be better to define a set of operations that 
-- work on tuplets rather than do ad hoc destructuring here
--  
take :: forall pch anno.
        Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
take i = viaNoteList (\_ xs -> step1 i xs)
  where
    step1 :: Int -> [StdElemNoteGroup2 pch anno]-> [StdElemNoteGroup2 pch anno]
    step1 n _   | n <= 0            = []
    step1 _ []                      = []
    step1 n (Atom e:es)             = (Atom e) : step1 (n-1) es
    step1 n (Tuplet spec xs:es)     = 
        let (n1,ys) = step2 n xs 
            spec2 = spec         
        in Tuplet spec2 ys : step1 n1 es  -- TODO remake spec

    step2 n []                      = (n,[])
    step2 n (e:es) | n <= 0         = (n,[])
                   | otherwise      = let (n1,ys) = step2 (n-1) es
                                      in (n1,e:ys)

{-

take2 :: Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
take2 i ph | i <= 0    = emptyOf ph
           | otherwise = step i $ viewl $ toLinear ph
  where
    -- note - counting down to 1 not zero
    step n (_ :< rest) 
         | n <= 1               = recalcBars $ fromLinear rest
         | otherwise            = step (n-1) $ viewl rest
    step _ Empty                = Phrase (phrase_header ph) []

-}

-- OLD - compare with Linear view version...

drop :: forall pch anno.
        Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
drop i = viaNoteList (\_ xs -> step1 i xs)
  where
    step1 :: Int -> [StdElemNoteGroup2 pch anno]-> [StdElemNoteGroup2 pch anno]
    step1 n xs | n <= 0             = xs
    step1 _ []                      = []
    step1 n (Atom _:es)             = step1 (n-1) es
    step1 n (Tuplet spec xs:es)     = case step2 n xs of
        Left n1 -> step1 n1 es
        Right ys -> let spec2 = spec 
                    in Tuplet spec2 ys : es  -- TODO remake spec

    step2 n []                      = Left n
    step2 n (_:es) | n <= 0         = Right es
                   | otherwise      = step2 (n-1) es
   

{-

-- Not yet accpetable...
-- This version is much clearer but it messes up if splitting Tuplets
--
-- Note - recalcs bars
--
drop2 :: Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
drop2 i ph | i <= 0    = ph
           | otherwise = step i $ viewl $ toLinear ph
  where
    -- note - counting down to 1 not zero
    step n (_ :< rest) 
         | n <= 1               = recalcBars $ fromLinear rest
         | otherwise            = step (n-1) $ viewl rest
    step _ Empty                = Phrase (phrase_header ph) []

-}


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
   

