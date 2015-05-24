{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.BeatPattern
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generate beat patterns
--
--------------------------------------------------------------------------------

module Payasan.Base.BeatPattern
  (
    BeatPattern
--  , beatPattern
  , beatPatternBars -- TEMP
--  , beatPatternFw

  , unit_bar
  , basic

  , divisions
  , arbitrary

  , nbars
    
  ) where


import Payasan.Base.Context
import Payasan.Base.Event
import Payasan.Base.EventList
import Payasan.Base.Internal.Base
import Payasan.Base.Internal.Beat
import Payasan.Base.Internal.Utils

import Control.Applicative
import Data.List ( foldl' )
import Data.Monoid




-- Helper

{-

-- | Impulses on rests are not preserved.
--
barsToImpulseMap :: [Bar a] -> ImpulseMap a
barsToImpulseMap = final . foldl' fbar (0, emptyHMap)
  where
    final (d,iim)               = buildImpulseMapH (TimeSpan 0 d) iim
    
    fbar (d,imm) b              = foldl' fdiv (d,imm) $ getBar b

    fdiv (d,imm) (GenBeat t1 a) = (d + t1, insertHMap d a imm)

-}

-- Maybe SecDevision is the best type for internal construction 
-- of meter patterns, before a final step turning into onsets...




--------------------------------------------------------------------------------
-- Beat Pattern monad

-- Just writer? - 
-- No, add tempo and time sig (as reader or as state? - state)

data St = St { st_time_sig :: TimeSig, st_tempo :: !BPM }
  deriving (Eq,Show)

newtype GenBeatPattern impl a = GenBeatPattern {
    getGenBeatPattern :: St -> (St, H (Bar impl), a) }


type BeatPattern a = GenBeatPattern a ()

instance Functor (GenBeatPattern impl) where
  fmap f ma = GenBeatPattern $ \s -> 
                let (s1,w1,a) = getGenBeatPattern ma s in (s1,w1,f a)


instance Applicative (GenBeatPattern impl) where
  pure a    = GenBeatPattern $ \s -> (s,emptyH, a)
  mf <*> ma = GenBeatPattern $ \s -> 
                let (s1,w1,f) = getGenBeatPattern mf s
                    (s2,w2,a) = getGenBeatPattern ma s1
                in (s2, w1 `appendH` w2, f a)

instance Monad (GenBeatPattern impl) where
  return    = pure
  ma >>= k  = GenBeatPattern $ \s -> 
                let (s1,w1,a) = getGenBeatPattern ma s
                    (s2,w2,b) = getGenBeatPattern (k a) s1
                in (s2, w1 `appendH` w2, b)


instance Monoid a => Monoid (GenBeatPattern impl a) where
  mempty          = pure mempty
  ma `mappend` mb = GenBeatPattern $ \s -> 
                      let (s1,w1,a) = getGenBeatPattern ma s
                          (s2,w2,b) = getGenBeatPattern mb s1
                      in (s2, w1 `appendH` w2, a <> b)


{-

beatPattern :: TimeSig -> BPM -> BeatPattern a -> ImpulseMap a
beatPattern ts bpm ma = 
    let st_zero = St { st_time_sig = ts, st_tempo = bpm }
        (_,w,_) = getGenBeatPattern ma st_zero
    in barsToImpulseMap $ toListH w

-}


-- TEMP
beatPatternBars :: TimeSig -> BPM -> BeatPattern a -> [Bar a]
beatPatternBars ts bpm ma = 
    let st_zero = St { st_time_sig = ts, st_tempo = bpm }
        (_,w,_) = getGenBeatPattern ma st_zero
    in toListH w

{-

beatPatternFw :: BeatPattern a -> (a -> EventFw uctx z) -> EventList uctx ()
beatPatternFw ma mf = ask_meter_info >>= \(MeterInfo ts bpm) ->
    let imap = beatPattern ts bpm ma 
    in impulsesFw imap mf

-}

tell :: Bar impl -> GenBeatPattern impl ()
tell b = GenBeatPattern $ \s -> (s, wrapH b, ())

getTempo :: GenBeatPattern impl BPM
getTempo = GenBeatPattern $ \s -> (s, emptyH, st_tempo s)


getTimeSig :: GenBeatPattern impl TimeSig
getTimeSig = GenBeatPattern $ \s -> (s, emptyH, st_time_sig s)


absUnit :: Seconds -> BeatPattern ()
absUnit d = tell $ Bar [ GenBeat d () ]

-- | Single beat.
--
unit_bar :: BeatPattern ()
unit_bar = getTimeSig >>= \ts  ->
           getTempo   >>= \bpm -> 
           absUnit $ barLength ts bpm
        

basic :: BeatPattern ()
basic = getTimeSig >>= \ts@(n,_) ->
        getTempo   >>= \bpm -> 
        tell $ makeBarFromDivisions ts bpm (makeSimpleDivs n)
  where
    makeSimpleDivs n = replicate n (GenBeat 1 ())


divisions :: [Rational] -> BeatPattern ()
divisions xs = getTimeSig >>= \ts  -> 
               getTempo   >>= \bpm -> 
               tell $ makeBarFromDivisions ts bpm $ readDivisions xs




arbitrary :: [Seconds] -> BeatPattern ()
arbitrary beat_lengths = 
    let divs = map (\drn -> GenBeat drn ()) beat_lengths 
    in tell $ Bar divs


--------------------------------------------------------------------------------
-- Introspection (censor)

censor :: ([Bar impl] -> [Bar impl]) 
       -> GenBeatPattern impl a -> GenBeatPattern impl a
censor fn ma = GenBeatPattern $ \s -> 
    let (s1,w1,a) = getGenBeatPattern ma s
        xs        = fn $ toListH w1
    in (s1, fromListH xs, a)


-- | truncate if long, cycle if short
nbars :: Int -> BeatPattern a -> BeatPattern a
nbars n = censor fn
  where
    fn [] = []
    fn xs = take n $ cycle xs

