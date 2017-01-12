{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventBar.FromIRSimpleTile
-- Copyright   :  (c) Stephen Tetley 2016-2017
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
    
    fromIRSimpleTile

  ) where



-- import Payasan.PSC.Repr.IRSimpleTile.Coalesce
import Payasan.PSC.Repr.IRSimpleTile.Syntax
import qualified Payasan.PSC.Repr.IREventBar.Syntax as T

import Payasan.Base.Basis (Seconds)

import qualified Data.List as List

type Onset = Seconds

    

fromIRSimpleTile :: Part pch anno -> T.Part pch anno
fromIRSimpleTile = partT


partT :: Part pch anno -> T.Part pch anno
partT (Part { part_sections = ss }) = 
    T.Part { T.part_sections = map sectionT ss }


sectionT :: Section pch anno -> T.Section pch anno
sectionT (Section { section_name  = name
                  , section_onset = ot
                  , section_bars  = bs }) =
    T.Section { T.section_name  = name
              , T.section_onset = ot
              , T.section_bars  = map barT bs 
              }

  

    
-- | At the point of generating a Bar we don't know the bar_onset.
--
barT :: Bar pch anno -> T.Bar pch anno
barT (Bar { bar_onset = ot
          , bar_elems = es })   =
    T.Bar { T.bar_onset  = ot
          , T.bar_events = concat $ snd $ List.mapAccumL elementA 0 es
          }
        




-- | Graces and chords generate more-then-one event
-- 
-- Onsets for elements within a bar are deltas from the onset 
-- of the bar.
-- They are not absolute times.
--
elementA :: Onset -> Element pch anno -> (Onset, [T.Event pch anno])

elementA ot (Note drn pch anno)             = 
    let evt  = T.Event ot pch drn anno
    in (ot + drn,[evt])

elementA ot (Rest drn)                      = (ot + drn,[])
    
elementA ot (Chord drn ps anno)             = 
    let evts = map (\p -> T.Event ot p drn anno) ps
    in (ot + drn,evts)

elementA ot (Graces ns)                     = 
    let step = \ons (drn,pch) -> 
                 let evt = T.Grace ons pch drn 
                 in (ons + drn, evt)
    in List.mapAccumL step ot ns

elementA ot (TiedCont drn)                  = (ot + drn,[])

