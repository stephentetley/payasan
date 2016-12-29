{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventBar.FromIRSimpleTile
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

module Payasan.PSC.Repr.IREventBar.FromIRSimpleTile
  ( 
    MakeEventDef(..)
  , fromIRSimpleTile

  ) where



-- import Payasan.PSC.Repr.IRSimpleTile.Coalesce
import Payasan.PSC.Repr.IRSimpleTile.Syntax
import qualified Payasan.PSC.Repr.IREventBar.Syntax as T

import Payasan.Base.Basis (Seconds)

import qualified Data.List as List

type Onset = Seconds

data MakeEventDef pch anno evt = MakeEventDef
    { makeEvent   :: pch -> Seconds -> anno -> evt 
    , graceNoAnno :: anno
    }
    

fromIRSimpleTile :: MakeEventDef pch anno evt 
                 -> Part pch anno 
                 -> T.Part Seconds evt
fromIRSimpleTile = partT

-- Notes
-- Although IRSimpleTile is a tiled representation there is a 
-- subtlety that bars are "mis-shaped" (actually mis-sized) 
-- after joining tied notes together.
-- To get "true" bar durations (and thus onsets) we have to look 
-- at IRSimpleTile before we have perfromed tie-joining.

  
-- | Note - we find onsets before we join ties.
--
partT :: MakeEventDef pch anno evt -> Part pch anno -> T.Part Seconds evt
partT def (Part { part_sections = ss }) = 
    T.Part { T.part_sections = map (sectionT def) ss }


sectionT :: MakeEventDef pch anno evt -> Section pch anno -> T.Section Seconds evt
sectionT def (Section { section_name  = name
                      , section_onset = ot
                      , section_bars  = bs }) =
    T.Section { T.section_name  = name
              , T.section_onset = ot
              , T.section_bars  = map (barT def) bs 
              }

  

    
-- | At the point of generating a Bar we don't know the bar_onset.
--
barT :: MakeEventDef pch anno evt 
     -> Bar pch anno 
     -> T.Bar Seconds evt
barT def (Bar { bar_onset = ot
              , bar_elems = es })   =
    T.Bar { T.bar_onset  = ot
          , T.bar_events = concat $ snd $ List.mapAccumL (elementT def) 0 es
          }
        




-- | Graces and chords generate more-then-one event
-- 
-- Onsets for elements within a bar are deltas from the onset 
-- of the bar.
-- They are not absolute times.
--
elementT :: MakeEventDef pch anno evt
         -> Onset 
         -> Element pch anno 
         -> (Onset, [T.Event Seconds evt])

elementT mkE ot (Note drn pch anno)             = 
    let evt  = makeEvent1 mkE ot pch drn anno
    in (ot + drn,[evt])

elementT _   ot (Rest drn)                      = (ot + drn,[])
    
elementT mkE ot (Chord drn ps anno)             = 
    let evts = map (\p -> makeEvent1 mkE ot p drn anno) ps
    in (ot + drn,evts)

elementT mkE ot (Graces ns)                     = 
    let anno = graceNoAnno mkE
        step = \ons (drn,pch) -> 
                 let evt = makeEvent1 mkE ons pch drn anno
                 in (ons + drn, evt)
    in List.mapAccumL step ot ns

elementT _   ot (TiedCont drn)                  = (ot + drn,[])


    
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
      

