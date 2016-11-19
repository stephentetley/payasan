{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IRSimpleTileToIREventBeam
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translate IRSimpleTile To IREventBeam.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IRSimpleTileToIREventBeam
  ( 
    MakeEventDef(..)
  , transIRSimpleTileToIREventBeam

  ) where



import Payasan.PSC.Repr.IRSimpleTile.Syntax
import Payasan.PSC.Repr.IRSimpleTile.Coalesce
import qualified Payasan.PSC.Repr.IREventBeam.Syntax as T

import Payasan.Base.Basis (Seconds)

import qualified Data.List as List

type Onset = Seconds

data MakeEventDef pch anno evt = MakeEventDef
    { makeEvent :: pch -> Seconds -> anno -> evt 
    , graceNoAnno :: anno
    }
    

transIRSimpleTileToIREventBeam :: MakeEventDef pch anno evt 
                               -> Part pch anno
                               -> T.Part Seconds evt
transIRSimpleTileToIREventBeam = partT

-- Notes
-- Although IRSimpleTile is a tiled representation there is a 
-- subtlety that bars are "mis-shaped" (actually mis-sized) 
-- after joining tied notes together.
-- To get "true" bar durations (and thus onsets) we have to look 
-- at IRSimpleTile before we have perfromed tie-joining.

  
-- | Note - we find onsets before we join ties.
--
partT :: MakeEventDef pch anno evt -> Part pch anno -> T.Part Seconds evt
partT mkE part = 
    let onsets = barOnsets part
        bars1  = barT mkE $ part_bars $ joinTies part
    in Part { part_bars = annotateOnsets onsets bars1 }
    

barOnsets :: Part pch anno -> [Seconds]
barOnsets (Part xs) = step 0 xs
  where
    -- onset is produced at the start of a bar 
    -- Both empty and one are recursion terminators
    -- (empty is only reached if we had no bars in the first place)
    step1 ac []     = []
    step1 ac [b]    = ac
    step1 ac (b:bs) = ac : step (ac + barD b) bs

    barD (Bar es) = sum $ map elementLength es


-- | List should be same length so we can trust zipWith not to 
-- truncate.
--
annotateOnsets :: [Seconds] -> [T.Bar Seconds evt] -> [T.Bar Seconds evt]
annotateOnsets = zipWith (\ot b -> b { T.bar_onset = ot })
  

    

    
-- | At the point of generating a Bar we don't know the bar_onset.
--
barT :: MakeEventDef pch anno evt 
     -> Bar pch anno 
     -> T.Bar Seconds evt
barT mkE (Bar _ gs)                  =
    let (dt,eventss) = List.mapAccumL (noteGroupT mkE) 0 gs
    in T.Bar { T.bar_onset = 0, T.bar_events = concat eventss }
           




-- | Graces and chords generate more-then-one event
-- 
-- Onsets for elements within a bar are deltas from the onset 
-- of the bar.
-- They are not absolute times.
--
elementT :: MakeEventDef pch anno evt
         -> Onset 
         -> Element pch Seconds anno 
         -> (Onset, [T.Event Seconds evt])
elementT mkE ot (Note drn pch anno _)             = 
    let evt  = makeEvent1 mkE ot pch drn anno
    in (ot + drn,[evt])

elementT _   ot (Rest drn)                        = (ot + drn,[])
    
elementT mkE ot (Chord drn ps anno _)             = 
    let evts = map (\p -> makeEvent1 mkE ot p drn anno) ps
    in (ot + drn,evts)

elementT mkE ot (Graces ns)                       = 
    let anno = graceNoAnno mkE
        step = \ons (drn,pch) -> 
                 let evt = makeEvent1 mkE ons pch drn anno
                 in (ons + drn, evt)
    in List.mapAccumL step ot ns

    
-- | Make an event. Chords, graces and note all generate events 
-- in the same way.
makeEvent1 :: MakeEventDef pch anno evt 
           -> Onset 
           -> pch 
           -> Seconds 
           -> anno 
           -> T.Event Seconds evt
makeEvent1 mkE ot pch drn anno = 
    T.Event { T.event_onset = ot
            , T.event_body  = (makeEvent mkE) pch drn anno }
      
