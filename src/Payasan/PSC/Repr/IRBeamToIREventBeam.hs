{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IRBeamToIREventBeam
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translate IRBeam To IREventBeam.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IRBeamToIREventBeam
  ( 
    MakeEventDef(..)
  , transIRBeamToIREventBeam

  ) where


import qualified Payasan.PSC.Repr.IREventBeam.Syntax as T
import Payasan.PSC.Repr.IRBeam.Syntax

import qualified Data.List as List


data MakeEventDef pch drn anno ot note = MakeEventDef
    { durationToOnset   :: drn -> ot
    , makeEvent         :: pch -> anno -> note
    }



transIRBeamToIREventBeam :: Num ot
                         => MakeEventDef pch drn anno ot note 
                         -> Part pch drn anno 
                         -> T.Part ot drn note
transIRBeamToIREventBeam = partT


-- TODO - how to render ties?...

partT :: Num ot => MakeEventDef pch drn anno ot note -> Part pch drn anno -> T.Part ot drn note
partT mkE (Part bs)               = 
    T.Part { T.part_bars = snd $ List.mapAccumL (barT mkE) 0 bs }


barT :: Num ot 
     => MakeEventDef pch drn anno ot note 
     -> ot -> Bar pch drn anno -> (ot, T.Bar ot drn note)
barT mkE ot (Bar _ gs)                  =
    let (dt,eventss) = List.mapAccumL (noteGroupT mkE) 0 gs
    in (ot + dt, T.Bar { T.bar_onset = ot, T.bar_events = concat eventss })
           


noteGroupT :: Num ot 
           => MakeEventDef pch drn anno ot note 
           -> ot -> NoteGroup pch drn anno -> (ot, [T.Event ot drn note])
noteGroupT mkE ot (Atom e)              = elementT mkE ot e
noteGroupT mkE ot (Beamed grps)         = 
    let (ot1, xss) = List.mapAccumL (noteGroupT mkE) ot grps in (ot1, concat xss)
--    | Tuplet   TupletSpec            [NoteGroup pch drn anno]



elementT :: Num ot
         => MakeEventDef pch drn anno ot note 
         -> ot -> Element pch drn anno -> (ot, [T.Event ot drn note])
elementT mkE ot (NoteElem (Note pch drn)  anno _) = 
    let ot1 = ot + (durationToOnset mkE) drn
        note = (makeEvent mkE) pch anno
        evt  = T.Event { T.event_delta_time = ot
                       , T.event_duration   = drn
                       , T.event_note       = note }
    in (ot1,[evt])

elementT mkE ot (Rest drn)                        = 
    let ot1 = ot + (durationToOnset mkE) drn in (ot1,[])

elementT mkE ot (Spacer drn)                      = 
    let ot1 = ot + (durationToOnset mkE) drn in (ot1,[])

elementT mkE ot (Skip drn)                        = 
    let ot1 = ot + (durationToOnset mkE) drn in (ot1,[])


--    | Chord         [pch]           drn   anno  Tie
--    | Graces        [Note pch drn]

elementT _   ot (Punctuation {})                  = (ot, [])
