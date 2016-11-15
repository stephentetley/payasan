{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.External.Clocktime
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Transform duration to a clocktime representation (aka Seconds).
-- 
-- TODO - coalesce ties etc.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.External.Clocktime
  ( 

    transClocktime

  ) where


import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Basis
import Payasan.Base.Duration

import Data.Ratio




--------------------------------------------------------------------------------
-- Coalesce tied notes and chords

-- First step linearize and turn duration to seconds
-- need to be in lear form to concat tied notes chords across 
-- bar lines / note groups



transClocktime :: Eq pch 
               => Part pch Duration anno -> Part pch Seconds anno
transClocktime p = 
    let metrics = extractBarInfos p
        notelist = coalesceTies $ linearize p
    in rebuildBars metrics notelist



noteDuration :: BPM -> Duration -> Seconds
noteDuration bpm d = 
    realToFrac (toRDuration d) * (4 * quarterNoteLength bpm)


--------------------------------------------------------------------------------
-- Linearize to a stream

linearize :: Part pch Duration anno -> [Element pch Seconds anno]
linearize (Part bs) = concatMap linearizeB bs

linearizeB :: Bar pch Duration anno -> [Element pch Seconds anno]
linearizeB (Bar info cs) = 
    let bpm = section_bpm info in concatMap (linearizeNG bpm) cs


linearizeNG :: BPM -> NoteGroup pch Duration anno -> [Element pch Seconds anno]
linearizeNG bpm (Atom e)            = [linearizeE bpm e]
linearizeNG bpm (Beamed es)         = concatMap (linearizeNG bpm) es
linearizeNG bpm (Tuplet spec es)    = map (scaleD (t%n)) $ concatMap (linearizeNG bpm) es
  where
    (TupletSpec t n _) = spec


linearizeE :: BPM -> Element pch Duration anno -> Element pch Seconds anno
linearizeE bpm (NoteElem e a t)     = NoteElem (linearizeN bpm e) a t
linearizeE bpm (Rest d)             = Rest $ noteDuration bpm d
linearizeE bpm (Spacer d)           = Spacer $ noteDuration bpm d
linearizeE bpm (Skip d)             = Skip $ noteDuration bpm d
linearizeE bpm (Chord ps d a t)     = Chord ps (noteDuration bpm d) a t
linearizeE bpm (Graces ns)          = Graces $ map (linearizeN bpm) ns
linearizeE _   (Punctuation s)      = Punctuation s


linearizeN :: BPM -> Note pch Duration -> Note pch Seconds
linearizeN bpm (Note pch drn)   = Note pch $ noteDuration bpm drn




-- Simplistic scaling of Tuplets - does this really work?
--
scaleD :: Ratio Int -> Element pch Seconds anno -> Element pch Seconds anno
scaleD sc elt = step (realToFrac sc) elt
  where
    step x (NoteElem n a t)     = NoteElem (note x n) a t
    step x (Rest d)             = Rest $ x * d
    step x (Spacer d)           = Spacer $ x * d
    step x (Skip d)             = Skip $ x * d
    step x (Chord ps d a t)     = Chord ps (x * d) a t
    step x (Graces ns)          = Graces $ map (note x) ns
    step _ (Punctuation s)      = Punctuation s

    note x (Note p d)           = Note p (x * d)


--------------------------------------------------------------------------------
-- Coalesce ties

coalesceTies :: Eq pch => [Element pch Seconds anno] -> [Element pch Seconds anno]
coalesceTies []     = []
coalesceTies (x:xs) = step x xs
  where
    step a []     = [a]
    step a (b:bs) = case together a b of
                      Nothing -> a : step b bs
                      Just t -> step t bs


-- Join together notes or chords if tied (and have the same notes).
--
together :: Eq pch
         => Element pch Seconds anno 
         -> Element pch Seconds anno 
         -> Maybe (Element pch Seconds anno)
together (NoteElem n1 _ t1)     (NoteElem n2 a t2)    = 
    case together1 n1 n2 t1 of
      Just note -> Just $ NoteElem note a t2
      Nothing -> Nothing

together (Chord ps1 d1 _ TIE)   (Chord ps2 d2 a t)    = 
    if ps1 == ps2 then Just $ Chord ps2 (d1+d2) a t
                  else Nothing

together _                      _                     = Nothing



-- Together for notes...
--
together1 :: Eq pch
          => Note pch Seconds 
          -> Note pch Seconds 
          -> Tie 
          -> Maybe (Note pch Seconds)
together1 (Note p1 d1) (Note p2 d2) t 
    | p1 == p2 && t == TIE   = Just $ Note p1 (d1+d2)
    | otherwise              = Nothing


--------------------------------------------------------------------------------
-- Divide into bars laxly 
--
-- Allow bars to be longer than the time given 
-- by their time signature.


rebuildBars :: [SectionInfo] -> [Element pch Seconds anno] -> Part pch Seconds anno
rebuildBars infos es = Part { part_bars = divideToBars 0 infos es }

type Carry = Seconds

-- | Note this can run out of bars even if elements still exist 
-- (in practice this shouldn't happen).
-- 
-- We may run out of elements and still have bars, in which case 
-- we produce empty bars.
--
divideToBars :: Carry -> [SectionInfo] -> [Element pch Seconds anno] -> [Bar pch Seconds anno]
divideToBars _ []     _  = []
divideToBars c (i:is) es = 
    let (bar,c1,rest) = extractSingleBar c i es
    in bar : divideToBars c1 is rest
    
extractSingleBar :: Carry -> SectionInfo -> [Element pch Seconds anno] -> (Bar pch Seconds anno, Carry, [Element pch Seconds anno])
extractSingleBar c info [] = let bar = Bar {bar_info = info, bar_groups = []} in (bar, c, [])


    
-- probably use a datatype to indicate 'carry'...
    