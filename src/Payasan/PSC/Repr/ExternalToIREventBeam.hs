{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.ExternalToIREventBeam
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translate External To IREventBeam.
--
-- External must have be already translated to have Seconds 
-- (duration). 
--
-- Tied notes must have been coalesced (it does not matter that 
-- a bar may now be ill-timed). 
--
-- Tuplets and graces-plus-successor must have their correct 
-- times calculated.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.ExternalToIREventBeam
  ( 
    MakeEventDef(..)
  , transExternalToIREventBeam

  ) where



import Payasan.PSC.Repr.External.Syntax
import qualified Payasan.PSC.Repr.IREventBeam.Syntax as T

import Payasan.Base.Basis (Seconds)

import qualified Data.List as List

type Onset = Seconds

data MakeEventDef pch anno evt = MakeEventDef
    { makeEvent :: pch -> Seconds -> anno -> evt 
    , graceNoAnno :: anno
    }
    

transExternalToIREventBeam :: MakeEventDef pch anno evt 
                           -> Part pch Seconds anno 
                           -> T.Part Seconds evt
transExternalToIREventBeam = partT



partT :: MakeEventDef pch anno evt -> Part pch Seconds anno -> T.Part Seconds evt
partT mkE (Part bs)               = 
    T.Part { T.part_bars = snd $ List.mapAccumL (barT mkE) 0 bs }


    
 -- TODO - must be sure we are handling empty bars correctly.
barT :: MakeEventDef pch anno evt 
     -> Onset 
     -> Bar pch Seconds anno 
     -> (Onset, T.Bar Seconds evt)
barT mkE ot (Bar _ gs)                  =
    let (dt,eventss) = List.mapAccumL (noteGroupT mkE) 0 gs
    in (ot + dt, T.Bar { T.bar_onset = ot, T.bar_events = concat eventss })
           


-- Assumes we have time-transformed tuplets to their correct 
-- clocktime duration (i.e tuplet spec has already been 
-- interpreted and is now redundant).
--
noteGroupT :: MakeEventDef pch anno evt
           -> Onset 
           -> NoteGroup pch Seconds anno 
           -> (Onset, [T.Event Seconds evt])
noteGroupT mkE ot (Atom e)              = elementT mkE ot e

noteGroupT mkE ot (Beamed grps)         = 
    let (ot1, xss) = List.mapAccumL (noteGroupT mkE) ot grps in (ot1, concat xss)

noteGroupT mkE ot (Tuplet _ grps)       = 
    let (ot1, xss) = List.mapAccumL (noteGroupT mkE) ot grps in (ot1, concat xss)



-- Translating an element may generate none (rest etc.), 
-- one (note) or multiple (chord) events.
-- Like Tuplets (in NoteGroup), we assume grace notes and their 
-- successor have been time transformed to their correct 
-- clocktime duration.
-- 
elementT :: MakeEventDef pch anno evt
         -> Onset 
         -> Element pch Seconds anno 
         -> (Onset, [T.Event Seconds evt])
elementT mkE ot (NoteElem (Note pch drn)  anno _) = 
    let evt  = makeEvent1 mkE ot pch drn anno
    in (ot + drn,[evt])

elementT _   ot (Rest drn)                        = (ot + drn,[])

elementT _   ot (Spacer drn)                      = (ot + drn,[])

elementT _   ot (Skip drn)                        = (ot + drn,[])

elementT mkE ot (Chord ps drn anno _)             = 
    let evts = map (\p -> makeEvent1 mkE ot p drn anno) ps
    in (ot + drn,evts)
    
elementT mkE ot (Graces ns)                       = 
    let anno = graceNoAnno mkE
        step = \ons (Note pch drn) -> 
                 let evt = makeEvent1 mkE ons pch drn anno
                 in (ons + drn, evt)
    in List.mapAccumL step ot ns
    
elementT _   ot (Punctuation {})                  = (ot, [])


makeEvent1 :: MakeEventDef pch anno evt 
           -> Onset 
           -> pch 
           -> Seconds 
           -> anno 
           -> T.Event Seconds evt
makeEvent1 mkE ot pch drn anno = 
    T.Event { T.event_onset = ot
            , T.event_body  = (makeEvent mkE) pch drn anno }
      
