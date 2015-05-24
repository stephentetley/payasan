{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.EventList
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Event lists.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.EventList
  ( 


    EventList
  , runEventList
  , evalEventList
  , renderEventList
  , writeEventListMF0

  , displace
  
  , event
  , event_

  , chord
  , chord_

  , linear 

  , trill
  , trill_
  , accTrill
  , accTrill_
  , decTrill
  , decTrill_

  )  where

import Payasan.Base.Concat
import Payasan.Base.Context
import Payasan.Base.Event
import Payasan.Base.Internal.Base
import Payasan.Base.Internal.Midi
import Payasan.Base.Internal.MidiOutput
import Payasan.Base.Internal.Utils


import Control.Applicative
import Control.Monad
import Data.Monoid




type InitialPos = Seconds

data Acc = Acc
    { acc_duration     :: !Seconds
    , acc_output_notes :: !MidiNoteList
    }

instance Monoid Acc where
  mempty = Acc { acc_duration     = 0
               , acc_output_notes = emptyH
               }

  Acc a1 b1 `mappend` Acc a2 b2 = 
      Acc { acc_duration     = max a1 a2
          , acc_output_notes = b1 `appendH` b2
          }

data Ans a = Ans !Acc a



instance Functor Ans where
  fmap f (Ans w a) = Ans w (f a)


-- If Advance has Context then EventList show have Context...

newtype EventList a = EventList { 
    getEventList :: Ctx -> InitialPos -> Acc -> Ans a }



instance Functor EventList where
  fmap f ma = EventList $ \r start ac -> fmap f $ getEventList ma r start ac



instance Applicative EventList where
  pure a    = EventList $ \_ _     ac -> Ans ac a
  mf <*> ma = EventList $ \r start ac -> 
                let Ans ac1 f = getEventList mf r start ac
                    Ans ac2 a = getEventList ma r start ac1
                in Ans ac2 (f a)

instance Monad EventList where
  return    = pure
  ma >>= k  = EventList $ \r start ac -> 
                let Ans ac1 a = getEventList ma r start ac
                in getEventList (k a) r start ac1


instance Monoid a => Monoid (EventList a) where
  mempty  = pure mempty
  mappend = overlay


instance ContextM EventList where
  askCtx        = EventList $ \r _     ac -> Ans ac r
  localize f ma = EventList $ \r start ac -> 
                      getEventList ma (f r) start ac



runEventList :: Ctx -> EventList a -> (TimeSpan, [MidiNote], a)
runEventList ctx ma = 
    let Ans (Acc drn notes) a = getEventList ma ctx 0 mempty
        tspan                 = TimeSpan 0 drn
    in (tspan, toListH notes, a) 


evalEventList :: Ctx -> EventList a -> [MidiNote]
evalEventList ctx ma = let (_,w,_) = runEventList ctx ma in w


renderEventList :: TrackData -> EventList a -> Track
renderEventList pfix ma = 
    let notes = evalEventList initialCtx ma 
    in render $ InterimTrack { track_config = pfix, track_notes = notes }


writeEventListMF0 :: FilePath -> TrackData -> EventList a -> IO ()
writeEventListMF0 path pfix ma =
    writeMF0 path $ renderEventList pfix ma


-- only direct concatenation is overlay
               
overlay :: Monoid a 
        => EventList a -> EventList a 
        -> EventList a
overlay ma mb = EventList $ \r start ac -> 
                    let Ans ac1 a = getEventList ma r start ac
                        Ans ac2 b = getEventList mb r start ac1
                    in Ans ac2 (a <> b)




displace :: Seconds -> EventList a -> EventList a
displace d ma = EventList $ \r start ac -> 
                  getEventList ma r (start + d) ac
                

-- horizontal concat is possible, but it would rely on timespan
-- being symbolically accurate vis note-lengths => wrong if notes
-- have sustain longer than their metric duration.




accDuration :: Onset -> Seconds -> TimeSpan -> Seconds
accDuration t0 d0 (TimeSpan t1 d1) = end - t0
  where
    end = max (t0 + d0) (t1 + d1)
   


-- Maybe event generates no impulse directly - join together 
-- with impulse if necessary (event_).


--
-- Potentially give event a tell-like type so that we can halve
-- the size of the API...
-- 
-- event :: Seconds -> Seconds -> Event a -> EventList ()
--


event :: Seconds -> Seconds -> Event a -> EventList a
event dt drn evt = EventList $ \r start (Acc d0 notes0)  -> 
    let pos              = start + dt
        (notes1,tspan,a) = runEvent evt r pos drn
        acc1  = Acc { acc_duration     = accDuration start d0 tspan
                    , acc_output_notes = notes0 `appendH` notes1
                    }
    in Ans acc1 a

event_ :: Seconds -> Seconds -> Event a -> EventList ()
event_ dt drn evt = void $ event dt drn evt

-- think we need forgetful _and_ memorable versions...




chord :: Monoid a => Seconds -> Seconds -> [pch] -> (pch -> Event a) -> EventList a
chord _  _   []     _  = return mempty
chord dt drn (x:xs) mf = event dt drn (mf x) >>= \a -> step a xs
  where
    step ac (p:ps) = event dt drn (mf p) >>= \a -> step (ac <> a) ps
    step ac []     = return ac


chord_ :: Seconds -> Seconds -> [pch] -> (pch -> Event a) -> EventList ()
chord_ dt drn xs mf = chord dt drn xs (void . mf)


linear :: Monoid a 
       => Seconds -> [Seconds] -> [pch] -> (pch -> Event a) -> EventList a
linear dt drns pchs mf = step dt mempty drns pchs
  where
    step _  ac []     _      = return ac

    step _  ac _      []     = return ac

    step ot ac ds     [p]    = event ot (sum ds) (mf p) >>= \a -> 
                               return (ac <> a)

    step ot ac (d:ds) (p:ps) = event ot d (mf p) >>= \a -> 
                               step (ot+d) (ac <> a) ds ps



-- | Obviously can supply the same pitch twice for a /tremolo/.
--
trill :: Monoid a 
      => Seconds -> Seconds -> pch -> pch -> Int -> (pch -> Event a) 
      -> EventList a
trill dt drn p1 p2 ndivs mf = linear dt ds ps mf
  where
    ds = equalLengths ndivs drn 
    ps = bireplicate ndivs p1 p2

trill_ :: Seconds -> Seconds -> pch -> pch -> Int -> (pch -> Event a) 
       -> EventList ()
trill_ dt drn p1 p2 ndivs mf = trill dt drn p1 p2 ndivs (void . mf)


-- | Obviously can supply the same pitch twice for a /tremolo/.
--
accTrill :: Monoid a 
         => Seconds -> Seconds -> pch -> pch -> Int -> (pch -> Event a) 
         -> EventList a
accTrill dt drn p1 p2 ndivs mf = linear dt ds ps mf
  where
    ds = acceleratingLengths ndivs drn 
    ps = bireplicate ndivs p1 p2


accTrill_ :: Seconds -> Seconds -> pch -> pch -> Int -> (pch -> Event a) 
          -> EventList ()
accTrill_ dt drn p1 p2 ndivs mf = accTrill dt drn p1 p2 ndivs (void . mf)

-- | Obviously can supply the same pitch twice for a /tremolo/.
--
decTrill :: Monoid a 
         => Seconds -> Seconds -> pch -> pch -> Int -> (pch -> Event a) 
         -> EventList a
decTrill dt drn p1 p2 ndivs mf = linear dt ds ps mf
  where
    ds = deceleratingLengths ndivs drn 
    ps = bireplicate ndivs p1 p2


decTrill_ :: Seconds -> Seconds -> pch -> pch -> Int -> (pch -> Event a) 
          -> EventList ()
decTrill_ dt drn p1 p2 ndivs mf = decTrill dt drn p1 p2 ndivs (void . mf)
