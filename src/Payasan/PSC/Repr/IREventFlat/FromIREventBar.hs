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
    GenEventAttrs(..)
  , fromIREventBar
  ) where

import Payasan.PSC.Repr.IREventBar.Syntax
import qualified Payasan.PSC.Repr.IREventFlat.Syntax as T

import Payasan.Base.Basis (Seconds)

data GenEventAttrs pch anno attrs = GenEventAttrs
    { genAttrsFromEvent        :: pch -> anno -> attrs
    , genAttrsFromGrace        :: pch -> attrs
    }



-- NOTE - there is no obligation to fix the type of Onset to
-- Seconds, although it is unlikely to be anything else. 

fromIREventBar :: GenEventAttrs pch anno attrs
               -> Part pch anno 
               -> T.Part Seconds Seconds attrs
fromIREventBar = partT


partT :: GenEventAttrs pch anno attrs
      -> Part pch anno 
      -> T.Part Seconds Seconds attrs
partT def (Part ss)                     = 
    T.Part { T.part_sections = map (sectionT def) ss }


sectionT :: GenEventAttrs pch anno attrs
         -> Section pch anno 
         -> T.Section Seconds Seconds attrs
sectionT def (Section { section_name = name
                      , section_bars = bs   })  = 
    T.Section { T.section_name = name
              , T.section_events = concatMap (barT def) bs
              }

barT :: GenEventAttrs pch anno attrs
     -> Bar pch anno 
     -> [T.Event Seconds Seconds attrs]
barT def (Bar ot cs)                = map (eventT def ot) cs



eventT :: GenEventAttrs pch anno attrs
       -> Seconds 
       -> Event pch anno 
       -> T.Event Seconds Seconds attrs
eventT def onsetb (Event o p d a)   = 
    let vals = (genAttrsFromEvent def) p a in T.Event (onsetb + o) d vals

eventT def onsetb (Grace o p d)     = 
    let vals = (genAttrsFromGrace def) p in T.Event (onsetb + o) d vals