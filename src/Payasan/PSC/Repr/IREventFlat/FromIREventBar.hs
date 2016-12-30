{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventFlat.FromIREventBar
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translate IREventBar to IREventFlat.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IREventFlat.FromIREventBar
  ( 
    fromIREventBar
  ) where

import Payasan.PSC.Repr.IREventBar.Syntax
import qualified Payasan.PSC.Repr.IREventFlat.Syntax as T

import Payasan.Base.Basis (Seconds)


-- NOTE - there is no obligation to fix the type of Onset to
-- Seconds, although it is unlikely to be anything else. 

fromIREventBar :: Part pch anno -> T.Part pch Seconds anno
fromIREventBar = partT


partT :: Part pch anno -> T.Part pch Seconds anno
partT (Part ss)                     = 
    T.Part { T.part_sections = map sectionT ss }

sectionT :: Section pch anno -> T.Section pch Seconds anno
sectionT (Section { section_name = name
                  , section_bars = bs   })  = 
    T.Section { T.section_name = name
              , T.section_events = concatMap barT bs
              }

barT :: Bar pch anno -> [T.Event pch Seconds anno]
barT (Bar ot cs)                    = map (eventT ot) cs

eventT :: Seconds -> Event pch anno -> T.Event pch Seconds anno
eventT onsetb (Event ot body)   = 
    T.Event { T.event_onset     = onsetb + ot
            , T.event_body      = eventBodyT body
            }

eventBodyT :: EventBody pch anno -> T.EventBody pch Seconds anno
eventBodyT (Event1 p d a)       = T.Event1 p d a
eventBodyT (EventGrace p d)     = T.EventGrace p d
