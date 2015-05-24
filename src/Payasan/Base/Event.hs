{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Event
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Events.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Event
  ( 

    EventWidth
  , Event
  , zeroEvent
  , zeroEventFw
  , makeEvent
  , makeEventFw
  , runEvent
  , displaceEvent

  )  where

import Payasan.Base.Context
import Payasan.Base.Internal.Base
import Payasan.Base.Internal.Midi
import Payasan.Base.Internal.Utils

import Control.Applicative
import Data.Monoid


type EventWidth = Seconds

--------------------------------------------------------------------------------




-- eventi writes answer as impulse:
--
-- > eventi :: Seconds -> Seconds -> Event impl -> EventList impl ()
-- 

data EventAns a = EventAns 
    { evt_ans_stmts     :: !MidiNoteList 
    , evt_ans_tspan     :: !TimeSpan 
    , evt_ans_result    :: !a
    }

-- Events are always supplied with /duration/ at the call site,
-- this is unlike text in PostScript where /width/ implicitly
-- stored in characters. Thus we don\'t return duration from an 
-- Event, because we have told the event the duration in the 
-- first place.




-- Events should support concat. 
-- We want to consider trills an event rather than an evenlist,
-- the same goes for chords, etc.


--
-- Widths and Durations
-- ====================
--
-- For some instruments, duration is the same as width 
-- (ideal width).
-- E.g. idealized synthesizer - plays note whilst key is pressed.
--
-- Some instruments have long decay - e.g. Bowed Bar.
-- The duration passed to Csound is fixed say three seconds, but 
-- the distance between one note and the next (the width) varies.
-- In a score "duration" is width.
-- TimeSpan is independent of score duration.
--
-- Some instruments have rapid attack - rapid decay, e.g. hi-hat.
-- The duration passed to Csound is fixed say a half a seconds, but 
-- the distance between one note and the next (the width) varies.
-- In a score "duration" is width.
-- TimeSpan is independent of score duration.
--
-- Some instruments have both ideal width and fixed width, e.g. 
-- a violin where bowing has ideal width and pizzicato has fixed
-- width.
--



type SymbolicDuration = Seconds

-- Functional params are onset and (metrical) duration

newtype Event a = Event { 
    getEvent :: Ctx -> Onset -> SymbolicDuration -> EventAns a }


-- A Functor instance lets us change 
-- @Event a@ to @Event ()@.
--
instance Functor Event where
  fmap f ea = Event $ \r ot drn -> 
      let EventAns stmts1 tspan1 a = getEvent ea r ot drn 
      in EventAns stmts1 tspan1 (f a)

-- Applicative, Monad ?
-- We want to allow compound Events (trills, tremolos,...) that
-- have staggered onsets dividing the duration. The obvious way
-- to do this is make Event a monad so we can query duration 
-- (as per a reader) and divide it.

instance Applicative Event where
  pure a    = zeroEvent a
  mf <*> ma = Event $ \r ot drn -> 
      let EventAns stmts1 tspan1 f = getEvent mf r ot drn
          EventAns stmts2 tspan2 a = getEvent ma r ot drn
      in EventAns { evt_ans_stmts     = stmts1 `appendH` stmts2
                  , evt_ans_tspan     = tspan1 `unionTimeSpan` tspan2
                  , evt_ans_result    = f a
                  }

instance Monad Event where
  return    = pure
  ma >>= k  = Event $ \r ot drn -> 
      let EventAns stmts1 tspan1 a = getEvent ma r ot drn
          EventAns stmts2 tspan2 b = getEvent (k a) r ot drn
      in EventAns { evt_ans_stmts     = stmts1 `appendH` stmts2
                  , evt_ans_tspan     = tspan1 `unionTimeSpan` tspan2
                  , evt_ans_result    = b
                  }

     

instance Monoid a => Monoid (Event a) where
  mempty           = zeroEvent mempty
  ma `mappend` mb  = Event $ \r ot drn -> 
      let EventAns stmts1 tspan1 a = getEvent ma r ot drn
          EventAns stmts2 tspan2 b = getEvent mb r ot drn
      in EventAns { evt_ans_stmts     = stmts1 `appendH` stmts2
                  , evt_ans_tspan     = tspan1 `unionTimeSpan` tspan2
                  , evt_ans_result    = a <> b
                  }
     



zeroEvent :: a -> Event a
zeroEvent ans = Event $ \_ ot drn -> 
    let tspan = TimeSpan ot drn 
    in EventAns { evt_ans_stmts     = emptyH
                , evt_ans_tspan     = tspan
                , evt_ans_result    = ans
                }

zeroEventFw :: a -> Seconds -> Event a
zeroEventFw ans fixd = Event $ \_ ot _ -> 
    let tspan = TimeSpan ot fixd 
    in EventAns { evt_ans_stmts     = emptyH
                , evt_ans_tspan     = tspan
                , evt_ans_result    = ans
                }



makeEvent :: a -> NoteValue -> Event a
makeEvent ans params = Event $ \_ ot drn -> 
    let tspan = TimeSpan ot drn
        note1 = MidiNote { note_start        = ot
                         , note_dur          = drn
                         , note_value        = params
                         }
    in EventAns { evt_ans_stmts     = wrapH note1 
                , evt_ans_tspan     = tspan
                , evt_ans_result    = ans
                }


-- | Can simplify fixed-width to just how an event is constructed.
--
makeEventFw :: a -> Seconds -> NoteValue -> Event a
makeEventFw ans fixd params = Event $ \_ ot _ -> 
    let tspan = TimeSpan ot fixd
        note1 = MidiNote { note_start        = ot
                         , note_dur          = fixd
                         , note_value        = params
                         }
    in EventAns { evt_ans_stmts     = wrapH note1 
                , evt_ans_tspan     = tspan
                , evt_ans_result    = ans
                }


-- 


runEvent :: Event a -> Ctx -> Onset -> Seconds 
         -> (MidiNoteList, TimeSpan, a)
runEvent ma ctx ot sdrn = 
    let EventAns hs ts a = getEvent ma ctx ot sdrn in (hs,ts,a)



displaceEvent:: Seconds -> Event a -> Event  a
displaceEvent d ma = Event $ \r start drn -> getEvent ma r (start + d) drn



