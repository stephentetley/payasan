{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IRBeam.AddBeams
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Beam grouping (notelists are already segmented into bars). 
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IRBeam.AddBeams
  (
    addBeams
  , noBeams
  ) where



import Payasan.PSC.Repr.IRBeam.Syntax
import Payasan.PSC.Base.SyntaxCommon
import Payasan.Base.Duration


import Data.Ratio



addBeams :: Part pch Duration anno -> Part pch Duration anno
addBeams (Part { part_bars = bs }) = Part $ map beamBar bs

beamBar :: Bar pch Duration anno -> Bar pch Duration anno
beamBar (Bar info cs) = 
    let mpat  = section_meter_pattern info
        segs1 = detachExtremities $ singleout $ segment mpat cs
    in Bar info $ beamSegments segs1


noBeams :: Part pch Duration anno -> Part pch Duration anno
noBeams = id






--------------------------------------------------------------------------------
-- Segment

-- This algo identifies /candidate/ groups for beaming, it does
-- not divide the bar strictly according to the meter pattern.
-- Due to /straddling/ ther may be more candidate groups than 
-- meter pattern divisions.

data InputRest pch drn anno = 
      GoodSplit [NoteGroup pch drn anno]
    | Straddle  RDuration  (NoteGroup pch drn anno)   [NoteGroup pch drn anno]



segment :: MeterPattern 
        -> [NoteGroup pch Duration anno] 
        -> [[NoteGroup pch Duration anno]]
segment []     xs = runOut xs
segment (d:ds) xs = let (seg1, rest) = segment1 d xs in
    case rest of 
       (GoodSplit ys) -> seg1 : segment ds ys
       (Straddle rightd y ys) -> seg1 : [y] : segment (decrease rightd ds) ys


segment1 :: RDuration 
         -> [NoteGroup pch Duration anno] 
         -> ([NoteGroup pch Duration anno], InputRest pch Duration anno)
segment1 _   []     = ([], GoodSplit [])
segment1 drn (x:xs) = step [] drn (x,sizeNoteGroup x) xs
  where
    step ac d (a,d1) cs@(b:bs) 
         | d1 <  d       = step (a:ac) (d - d1) (b, sizeNoteGroup b) bs
         | d1 == d      = (reverse (a:ac), GoodSplit cs)
         | otherwise    = (reverse ac,     Straddle (d1 - d) a cs)

    step ac d (a,d1) []        
         | d1 <= d      = (reverse (a:ac), GoodSplit [])
         | otherwise    = (reverse ac,     Straddle (d1 - d) a [])

runOut :: [NoteGroup pch Duration anno] -> [[NoteGroup pch Duration anno]]
runOut = map (\a -> [a])


decrease :: RDuration -> MeterPattern -> MeterPattern
decrease _ []         = []
decrease r (d:ds)     
    | r <  d          = (d - r) : ds
    | r == d          = ds
    | otherwise       = decrease (r - d) ds

--------------------------------------------------------------------------------
-- Single out long notes (quater notes or longer)


singleout :: [[NoteGroup pch Duration anno]] -> [[NoteGroup pch Duration anno]]
singleout = concatMap singleout1

singleout1 :: [NoteGroup pch Duration anno] -> [[NoteGroup pch Duration anno]]
singleout1 [] = []
singleout1 (x:xs) = step [] x xs
  where
    step ac a []        
        | isSmall a     = [ reverse (a:ac) ]
        | otherwise     = [reverse ac, [a]]

    step ac a (y:ys) 
        | isSmall a     = step (a:ac) y ys
        | otherwise     = (reverse ac) : [a] : step [] y ys


isSmall :: NoteGroup pch Duration anno -> Bool
isSmall a = sizeNoteGroup a < qtrnote_len

qtrnote_len :: RDuration 
qtrnote_len = (1%4)

--------------------------------------------------------------------------------
-- Detach extremities

--
-- Beam groups should not start or end with rests 
-- (and spacers if we add them).
--

-- | Lists of NoteGroup are so short in Bars that 
-- we dont care about (++).
--

detachExtremities :: [[NoteGroup pch Duration anno]] 
                  -> [[NoteGroup pch Duration anno]]
detachExtremities = concatMap detachBeamed


detachBeamed :: [NoteGroup pch Duration anno] 
             -> [[NoteGroup pch Duration anno]]
detachBeamed xs = 
    let (as,rest)       = frontAndRest xs
        (csr,middler)   = frontAndRest $ reverse rest
    in [as, reverse middler, reverse csr]
  where
    frontAndRest                    = span detachable


-- | If we already have a Tuplet or Beam group at the left or right
-- of the beam group we assume they are well formed
-- 
detachable :: NoteGroup pch drn anno -> Bool
detachable (Atom e) = detachableE e
detachable _        = False

detachableE :: Element pch drn anno -> Bool
detachableE (Rest {})           = True
detachableE (Spacer {})         = True
detachableE (Skip {})           = True
detachableE (NoteElem {})       = False
detachableE (Chord {})          = False
detachableE (Graces {})         = False
detachableE (Punctuation {})    = True

--------------------------------------------------------------------------------
-- Finally beam

-- | Beam segments with 2 or more members.
--
beamSegments :: [[NoteGroup pch Duration anno]] -> [NoteGroup pch Duration anno]
beamSegments []              = []
beamSegments ([]:xss)        = beamSegments xss
beamSegments ([x]:xss)       = x : beamSegments xss
beamSegments (xs:xss)        = Beamed xs : beamSegments xss
