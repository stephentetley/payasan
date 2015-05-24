{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Advance
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- An event list with cursor position and /advancing/.
--
-- This module is expected to be the building block for symbolic
-- note-lists.
--
--------------------------------------------------------------------------------

module Payasan.Base.Advance
  ( 
    Advance
  , CursorPos
  , runAdvance
  , evalAdvance

  , renderAdvance


  , event
  , event_

  , eventNoWidth
  , eventNoWidth_

  , chord
  , chord_

  , trill
  , trill_

  , advanceCursor

  ) where


import Payasan.Base.Context
import Payasan.Base.Event
import qualified Payasan.Base.EventList as E
import Payasan.Base.Internal.Base
import Payasan.Base.Internal.Midi


import Control.Applicative
import Control.Monad
import Data.Monoid


--
-- NOTES
--
-- Always use seconds (rather than something symbolic) then
-- we don\'t need to worry about interpreting time with the Ctx. 
--

type CursorPos = Seconds



newtype Advance a = Advance { 
    getAdvance :: CursorPos -> E.EventList (CursorPos, a) }



instance Functor (Advance) where
  fmap f ma = Advance $ \s -> 
                getAdvance ma s >>= \(s1,a) -> return (s1, f a)


instance Applicative (Advance) where
  pure a    = Advance $ \s -> return (s,a)
  mf <*> ma = Advance $ \s -> 
                getAdvance mf s   >>= \(s1,f) ->
                getAdvance ma s1  >>= \(s2,a) ->
                return (s2, f a)

instance Monad (Advance) where
  return    = pure
  ma >>= k  = Advance $ \s -> 
                getAdvance ma s >>= \(s1,a) -> getAdvance (k a) s1
            
instance Monoid a => Monoid (Advance a) where
  mempty    = Advance $ \s -> return (s, mempty)
  mappend   = nextTo




instance ContextM (Advance) where
  askCtx        = liftEventList askCtx
  localize f ma = Advance $ \s -> localize f (getAdvance ma s)


-- Once we have curpos pos, we can have a useful nextTo (hcat)...
nextTo :: Monoid a => Advance a -> Advance a -> Advance a
nextTo ma mb = Advance $ \s -> 
    getAdvance ma s   >>= \(s1,a) ->
    getAdvance mb s1  >>= \(s2,b) ->
    return (s2, a <> b)

liftEventList :: E.EventList a -> Advance a
liftEventList ma = Advance $ \s -> ma >>= \a -> return (s,a)


-- Advance is expected to used to implement more
-- user-friendly (musical) objects, hence we have a set of /run/ 
-- names cf. the state monad.
-- 

runAdvance :: Ctx -> Advance a -> (TimeSpan, [MidiNote], a)
runAdvance ctx ma = 
    let (ts,w,(_,a)) = E.runEventList ctx $ getAdvance ma 0 in (ts,w,a)
                     

evalAdvance :: Ctx -> Advance a -> [MidiNote]
evalAdvance ctx ma = let (_,w,_) = runAdvance ctx ma in w




renderAdvance :: TrackData -> Advance a -> Track
renderAdvance pfix ma = E.renderEventList pfix $ getAdvance ma 0


-- | Four operations are need to render symbolic note-lists.
-- 
-- 1. note - tell event and increase cursor pos.
-- 
-- 2. noteZeroWidth - tell event, do not increase cursor pos.
-- 
-- 3. advance - increase cursor pos (cf. a rest)
--
-- 4. impulse - tell an inpulse at current position
-- 
-- We use a prefix `tell` as client code may want the nice names
-- like note.
-- 
-- With these operations chords are several noteZeroWidth then an
-- advance of the chord duration.
--


-- This is a poor API - there should be a single param for 
-- width/duration, delegating to the event as to how it is 
-- interpreted.
--



event :: Seconds -> Event a -> Advance a
event drn ma = Advance $ \dt -> E.event dt drn ma >>= \a -> return (dt+drn, a)

event_ :: Seconds -> Event a -> Advance ()
event_ drn ma = void $ event drn ma 



eventNoWidth :: Seconds -> Event a -> Advance a
eventNoWidth drn ma = 
    Advance $ \dt -> E.event dt drn ma >>= \a -> return (dt, a)

eventNoWidth_ :: Seconds -> Event a -> Advance ()
eventNoWidth_ drn ma = void $ eventNoWidth drn ma


-- Could build chords from eventNoWidth...

chord :: Monoid a => Seconds -> [pch] -> (pch -> Event a) -> Advance a
chord drn xs mf = 
    Advance $ \dt -> E.chord dt drn xs mf >>= \a -> return (dt+drn, a)

chord_ :: Seconds -> [pch] -> (pch -> Event a) -> Advance ()
chord_ drn ps mf = chord drn ps (void . mf)



trill :: Monoid a 
      => Seconds -> pch -> pch -> Int -> (pch -> Event a) -> Advance a
trill drn p1 p2 ndivs mf = 
    Advance $ \dt -> E.trill dt drn p1 p2 ndivs mf >>= \a -> return (dt+drn, a)

trill_ :: Seconds -> pch -> pch -> Int -> (pch -> Event a) -> Advance ()
trill_ drn p1 p2 ndivs mf = trill drn p1 p2 ndivs (void . mf)



advanceCursor :: Seconds -> Advance ()
advanceCursor delta = Advance $ \s -> return (s+delta, ())

