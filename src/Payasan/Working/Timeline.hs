{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Timeline
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Metrical (bars and beats) timeline.
--
-- ScorePos - ** Indexing is 1 based **  (As per music not Haskell)
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Timeline
  ( 

    ScorePos
  , Timeline

  , default_timeline
  , regularTimeline

  -- Build
  , TimelineSpec
  , metered
  , freeMetered
  , timelineSpec


  -- Query
  , getPosition
  , getMetricalInfoAbs
  , getMetricalInfoPos



  -- Render 


  )  where



import Payasan.Base.Internal.Base
import Payasan.Base.Internal.Utils
import Payasan.Base.Trail

import Control.Applicative
import qualified Data.IntMap as IM
import Data.List ( unfoldr )
import Data.Monoid


type OnsetTime = Seconds


-- | ScorePos = (bar x beat).
--
type ScorePos = (Int,Int)


-- Regions should probably be named (for songs) 

-- TODO - if we extend timeline to have /default/ meter we can
-- get rid of some partial(-ish) functions upstream...
--
data Timeline = Timeline 
    { tl_default_meter    :: Meter
    , tl_regions          :: IM.IntMap Region 
    }




-- Free-metered can be achieved with a bar in 1/4 time 

-- | Don\'t need sequence lengths for regions in int map.
-- Only need to consider start times and their animations.
--
data Region = Region !OnsetTime !TimeSig !BPM
  deriving (Eq,Show)



default_timeline :: Timeline
default_timeline = Timeline { tl_default_meter  = Meter (4,4) 120
                            , tl_regions        = IM.empty
                            }

regularTimeline :: TimeSig -> BPM -> Timeline
regularTimeline ts bpm = 
    Timeline { tl_default_meter  = Meter ts bpm
             , tl_regions        = IM.empty
             }



--------------------------------------------------------------------------------
-- Build a Timeline

-- | Consider labelling positions of interest (anchors) at 
-- some point...
--
newtype TimelineSpec a = TimelineSpec { 
    getTimelineSpec :: Int -> (Int, H SpecRegion, a) }

-- | Tracks onset bar number, number of bars (/free-metered/ 
-- should be exactly 1 bar) and metrical info. 
--
data SpecRegion = SMetered  !Int  !Int     !TimeSig  !BPM
  deriving (Eq,Show)


instance Functor TimelineSpec where
  fmap f ma = TimelineSpec $ \s -> 
                let (s1,w1,a) = getTimelineSpec ma s in (s1,w1,f a)


instance Applicative TimelineSpec where
  pure a    = TimelineSpec $ \s -> (s, emptyH, a)
  mf <*> ma = TimelineSpec $ \s -> 
                let (s1,w1,f) = getTimelineSpec mf s
                    (s2,w2,a) = getTimelineSpec ma s1
                in (s2, w1 `appendH` w2, f a)

instance Monad TimelineSpec where
  return    = pure
  ma >>= k  = TimelineSpec $ \s -> 
                let (s1,w1,a) = getTimelineSpec ma s
                    (s2,w2,b) = getTimelineSpec (k a) s1
                in (s2, w1 `appendH` w2, b)


instance Monoid a => Monoid (TimelineSpec a) where
  mempty          = pure mempty
  ma `mappend` mb = TimelineSpec $ \s -> 
                      let (s1,w1,a) = getTimelineSpec ma s
                          (s2,w2,b) = getTimelineSpec mb s1
                      in (s2, w1 `appendH` w2, a <> b)


timelineSpec :: TimelineSpec a -> Timeline
timelineSpec ma = 
    let (_,w1,_) = getTimelineSpec ma 1 in buildTimeline $ toListH w1

metered :: Int -> TimeSig -> BPM -> TimelineSpec ()
metered nbars ts bpm = TimelineSpec $ \s -> 
    (s + nbars, wrapH $ SMetered s nbars ts bpm, () )

freeMetered :: Seconds -> TimelineSpec () 
freeMetered barlen = TimelineSpec $ \s -> 
    let bpm = bpmFromQnl barlen in (s + 1, wrapH $ SMetered s 1 (1,4) bpm, ())



-- | TODO - should this end with last element extended or use 
-- default meter after specified meter regions have run out?
-- 
buildTimeline :: [SpecRegion] -> Timeline
buildTimeline []      = default_timeline
buildTimeline (x:xs)  = step 0 emptyTl x xs
  where
    step dt ac r1 []     = ins2 dt r1 ac

    step dt ac r1 (r:rs) = 
        let (dt1, ac1) = ins1 dt r1 ac 
        in step dt1 ac1 r rs

    ins1 dt (SMetered bar numb ts bpm)  ac = 
        let bar_length  = barLength ts bpm
            tot_length  = fromIntegral numb * bar_length
        in (dt + tot_length, insertTl bar (Region dt ts bpm) ac)

    ins2 dt (SMetered bar _ ts bpm) ac = insertTl bar (Region dt ts bpm) ac


emptyTl :: Timeline
emptyTl = default_timeline

insertTl :: Int -> Region -> Timeline -> Timeline
insertTl bar reg tl@(Timeline { tl_regions = rgs }) = 
    tl { tl_regions = IM.insert bar reg rgs }


--------------------------------------------------------------------------------
-- Query

getPosition  :: ScorePos -> Timeline -> Seconds
getPosition (bar, beat) tl = case IM.lookupLE bar $ tl_regions tl of
    Nothing -> let (Meter ts bpm ) = tl_default_meter tl
               in relativePosition bar beat ts bpm

    Just (bstart, Region start ts bpm) -> 
        start + relativePosition (bar - bstart) beat ts bpm


relativePosition :: Int -> Int -> TimeSig -> BPM -> Seconds
relativePosition bar beat ts bpm = 
    let bar_len     = barLength  ts bpm
        beat_len    = beatLength ts bpm
    in fromIntegral (bar - 1) * bar_len  + fromIntegral (beat - 1) * beat_len


findRegionAbs :: Seconds -> Timeline -> Maybe Region
findRegionAbs dt tl = step Nothing $ IM.elems $ tl_regions tl
  where
    step ac []                                    = ac
    step ac ((Region onset _ _):_) | onset > dt   = ac
    step _  (x:xs)                                = step (Just x) xs

findRegionPos :: ScorePos -> Timeline -> Maybe Region
findRegionPos (scr_bar, _) tl = 
    step Nothing $ IM.toAscList $ tl_regions tl
  where
    step ac []                                    = ac
    step ac ((bar,_):_)   |  bar > scr_bar        = ac
    step _  ((_,r)  :xs)                          = step (Just r) xs



metricalInfo :: Timeline -> Maybe Region -> (TimeSig,BPM)
metricalInfo _  (Just (Region _ ts bpm))  = (ts,bpm)
metricalInfo tl (Nothing)                 = 
    let Meter ts bpm = tl_default_meter tl in (ts,bpm)

getMetricalInfoAbs :: Seconds -> Timeline -> (TimeSig,BPM)
getMetricalInfoAbs dt tl = metricalInfo tl $ findRegionAbs dt tl

getMetricalInfoPos :: ScorePos -> Timeline -> (TimeSig,BPM)
getMetricalInfoPos sp tl = metricalInfo tl $ findRegionPos sp tl


--------------------------------------------------------------------------------
-- animate



-- | Elaborate region with final bar (as well as start bar).
-- This means we can process regions one-by-one.
--
elaborate :: Int -> [(Int,Region)] -> [(Int,Int,Region)]
elaborate _    []                       = []
elaborate bmax ((b1,_):_) | b1 >  bmax  = []
elaborate bmax [(b1,final)]             = [(b1, bmax, final)]
elaborate bmax ((b1,r1):(b2,r2):xs)     = 
    let end = min bmax (b2 - 1)
    in (b1,end,r1) : elaborate bmax ((b2,r2) : xs)


-- | Note - bar range (bstart - bfinal) is inclusive, so we add1
-- to nbars
--
addBars :: (Int, Int, Region) -> Trail ScorePos
addBars (bstart, bfinal, Region start ts@(nbeats,_) bpm)   = 
    let nbars     = 1 + bfinal - bstart
        beat_len  = beatLength ts bpm
        xs        = zip (onsets start nbars nbeats beat_len) 
                        (posns bstart nbars nbeats)
    in mapM_ (uncurry insert) xs





onsets :: Seconds -> Int -> Int -> Seconds -> [Seconds]
onsets delta_start nbars nbeats beat_len = 
    take (nbars * nbeats) $ iterate (+ beat_len) delta_start 


posns :: Int -> Int -> Int -> [ScorePos]
posns delta_bar nbars nbeats = unfoldr fn (1,1) 
  where
    fn (i,_) | i > nbars  = Nothing
    fn (i,j) | j > nbeats = let pos = (delta_bar + i + 1, 1) 
                            in Just (pos, (i+1,1))
    fn (i,j)              = let pos = (delta_bar + i, j)
                            in Just (pos, (i,j+1))