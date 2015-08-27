{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.AddBeams
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Beam grouping (notelists are already segmented into bars). 
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.AddBeams
  (
    addBeams
  ) where



import Payasan.Base.Internal.BracketSyntax
import Payasan.Base.Duration


import Data.Ratio



addBeams :: Phrase pch -> Phrase pch
addBeams (Phrase { phrase_bars = bs }) = Phrase $ map beamBar bs

beamBar :: Bar pch -> Bar pch
beamBar (Bar info cs) = 
    let mpat  = render_meter_patn info
        segs1 = detachExtremities $ singleout $ segment mpat cs
    in Bar info $ beamSegments segs1



--------------------------------------------------------------------------------
-- Segment

-- This algo identifies /candidate/ groups for beaming, it does
-- not divide the bar strictly according to the meter pattern.
-- Due to /straddling/ ther may be more candidate groups than 
-- meter pattern divisions.

data InputRest pch = GoodSplit [CtxElement pch]
                   | Straddle  RDuration  (CtxElement pch)   [CtxElement pch]



segment :: MeterPattern -> [CtxElement pch] -> [[CtxElement pch]]
segment []     xs = runOut xs
segment (d:ds) xs = let (seg1, rest) = segment1 d xs in
    case rest of 
       (GoodSplit ys) -> seg1 : segment ds ys
       (Straddle rightd y ys) -> seg1 : [y] : segment (decrease rightd ds) ys


segment1 :: RDuration -> [CtxElement pch] -> ([CtxElement pch], InputRest pch)
segment1 _   []     = ([], GoodSplit [])
segment1 drn (x:xs) = step [] drn (x,sizeCtxElement x) xs
  where
    step ac d (a,d1) cs@(b:bs) 
         | d1 <  d       = step (a:ac) (d - d1) (b, sizeCtxElement b) bs
         | d1 == d      = (reverse (a:ac), GoodSplit cs)
         | otherwise    = (reverse ac,     Straddle (d1 - d) a cs)

    step ac d (a,d1) []        
         | d1 <= d      = (reverse (a:ac), GoodSplit [])
         | otherwise    = (reverse ac,     Straddle (d1 - d) a [])

runOut :: [CtxElement pch] -> [[CtxElement pch]]
runOut = map (\a -> [a])


decrease :: RDuration -> MeterPattern -> MeterPattern
decrease _ []         = []
decrease r (d:ds)     
    | r <  d          = (d - r) : ds
    | r == d          = ds
    | otherwise       = decrease (r - d) ds

--------------------------------------------------------------------------------
-- Single out long notes (quater notes or longer)


singleout :: [[CtxElement pch]] -> [[CtxElement pch]]
singleout = concatMap singleout1

singleout1 :: [CtxElement pch] -> [[CtxElement pch]]
singleout1 [] = []
singleout1 (x:xs) = step [] x xs
  where
    step ac a []        
        | isSmall a     = [ reverse (a:ac) ]
        | otherwise     = [reverse ac, [a]]

    step ac a (y:ys) 
        | isSmall a     = step (a:ac) y ys
        | otherwise     = (reverse ac) : [a] : step [] y ys


isSmall :: CtxElement pch -> Bool
isSmall a = sizeCtxElement a < qtrnote_len

qtrnote_len :: RDuration 
qtrnote_len = (1%4)

--------------------------------------------------------------------------------
-- Detach extremities

--
-- Beam groups should not start or end with rests 
-- (and spacers if we add them).
--

-- | Lists of CtxElement are so short in Bars that 
-- we dont care about (++).
--

detachExtremities :: [[CtxElement pch]] -> [[CtxElement pch]]
detachExtremities = concatMap detachBeamed


detachBeamed :: [CtxElement pch] -> [[CtxElement pch]]
detachBeamed xs = 
    let (as,rest)       = frontAndRest xs
        (csr,middler)   = frontAndRest $ reverse rest
    in [as, reverse middler, reverse csr]
  where
    frontAndRest                    = span detachable


-- | If we already have a Tuplet or Beam group at the left or right
-- of the beam group we assume they are well formed
-- 
detachable :: CtxElement pch -> Bool
detachable (Atom e) = detachableE e
detachable _        = False

detachableE :: Element pch -> Bool
detachableE (Rest {})     = True
detachableE (NoteElem {}) = False
detachableE (Chord {})    = False
detachableE (Graces {})   = False

--------------------------------------------------------------------------------
-- Finally beam

-- | Beam segments with 2 or more members.
--
beamSegments :: [[CtxElement pch]] -> [CtxElement pch]
beamSegments []              = []
beamSegments ([]:xss)        = beamSegments xss
beamSegments ([x]:xss)       = x : beamSegments xss
beamSegments (xs:xss)        = Beamed xs : beamSegments xss