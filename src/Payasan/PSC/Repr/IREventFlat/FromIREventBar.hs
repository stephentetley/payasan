{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventFlat.FromIREventBar
-- Copyright   :  (c) Stephen Tetley 2016-2017
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
    GenEventBody(..)
  , fromIREventBar
  ) where

import Payasan.PSC.Repr.IREventBar.Syntax
import qualified Payasan.PSC.Repr.IREventFlat.Syntax as T

import Payasan.Base.Basis (Seconds)

data GenEventBody pch anno body = GenEventBody
    { genBodyFromEvent        :: pch -> anno -> body
    , genBodyFromGrace        :: pch -> body
    }



-- NOTE - there is no obligation to fix the type of Onset to
-- Seconds, although it is unlikely to be anything else. 

fromIREventBar :: GenEventBody pch anno body
               -> Part pch anno 
               -> T.Part Seconds Seconds body
fromIREventBar = partT


partT :: GenEventBody pch anno body
      -> Part pch anno 
      -> T.Part Seconds Seconds body
partT def (Part ss)                     = 
    T.Part { T.part_sections = map (sectionT def) ss }


sectionT :: GenEventBody pch anno body
         -> Section pch anno 
         -> T.Section Seconds Seconds body
sectionT def (Section { section_name = name
                      , section_bars = bs   })  = 
    T.Section { T.section_name = name
              , T.section_events = concatMap (barT def) bs
              }

barT :: GenEventBody pch anno body
     -> Bar pch anno 
     -> [T.Event Seconds Seconds body]
barT def (Bar ot cs)                = map (eventT def ot) cs



eventT :: GenEventBody pch anno body
       -> Seconds 
       -> Event pch anno 
       -> T.Event Seconds Seconds body
eventT def onsetb (Event o p d a)   = 
    let vals = (genBodyFromEvent def) p a in T.Event (onsetb + o) d vals

eventT def onsetb (Grace o p d)     = 
    let vals = (genBodyFromGrace def) p in T.Event (onsetb + o) d vals