{-# LANGUAGE KindSignatures             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Context
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Context type (aka Reader env).
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Context
  ( 

    Ctx(..)
  , MeterInfo(..)
  , ContextF
  , initialCtx

  , ContextM(..)


  , set_metered
  , set_free_metered
  , set_bpm
  , set_time_sig

  , quarter_note_length
  , bar_length

  , durationInSeconds
  , interpretSeconds


  )  where


import Payasan.Base.Internal.Base

import Control.Applicative

-- Potentially we might want TimeSig in Ctx, it may be useful
-- for pattern generating /sequencers/. 
--
-- Remember - cannot use Haskell'\s Rational datatype for time
-- signatures as it normalizes, e.g. 2%2 == 4%4

data Ctx = Ctx 
    { ctx_bpm           :: !BPM
    , ctx_time_sig      :: !TimeSig
    }



-- | Free metered is 1/4 time with quarter-note length derived 
-- from BPM.
--
data MeterInfo = MeterInfo !TimeSig !BPM
  deriving (Eq,Show)


type ContextF = Ctx -> Ctx

initialCtx :: Ctx
initialCtx = Ctx { ctx_bpm         = 120
                 , ctx_time_sig    = (4,4)
                 }





class (Applicative m, Monad m) => ContextM (m :: * -> *) where
  askCtx        :: m Ctx
  asksCtx       :: (Ctx -> a) -> m a
  localize      :: ContextF -> m a -> m a

  asksCtx f     = f <$> askCtx



set_metered :: TimeSig -> BPM -> ContextF
set_metered ts bpm      = \s -> s { ctx_bpm = bpm, ctx_time_sig = ts }


set_free_metered :: Seconds -> ContextF
set_free_metered qnl = \s -> 
    s { ctx_bpm = bpmFromQnl qnl, ctx_time_sig = (1,4) } 


set_bpm :: BPM -> ContextF
set_bpm bpm = \s -> s { ctx_bpm = bpm}

set_time_sig :: TimeSig -> ContextF
set_time_sig ts = \s -> s { ctx_time_sig = ts}


quarter_note_length :: ContextM m => m Seconds
quarter_note_length = fn <$> asksCtx ctx_bpm
  where
    fn bpm = 60 / bpm


bar_length :: ContextM m => m Seconds
bar_length = barLength <$> asksCtx ctx_time_sig <*> asksCtx ctx_bpm



durationInSeconds :: ContextM m => Beat -> m Seconds 
durationInSeconds drn = (\bpm -> beatToSeconds bpm drn) <$> asksCtx ctx_bpm


interpretSeconds :: ContextM m => Seconds -> m Beat
interpretSeconds s = (\bpm -> secondsToBeat bpm s) <$> asksCtx ctx_bpm


