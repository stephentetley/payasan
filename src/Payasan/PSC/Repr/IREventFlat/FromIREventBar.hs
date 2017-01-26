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

  -- Version for MIDI which generates two events (Note-on, Note-off)
  , GenEventBody2(..)
  , fromIREventBar2

  ) where

import Payasan.PSC.Repr.IREventBar.Syntax
import qualified Payasan.PSC.Repr.IREventFlat.Syntax as T

import Payasan.Base.Basis (Seconds)

data GenEventBody pch anno body = GenEventBody
    { genBodyFromEvent        :: pch -> anno -> body
    , genBodyFromGrace        :: pch -> body
    }



-- NOTE - Onsets and Durations are not unit-changed duraing 
-- translation.
-- They remain in Seconds though they can be mapped to something 
-- else after translation. 

fromIREventBar :: GenEventBody pch anno body
               -> Part pch anno 
               -> T.Part Seconds Seconds body
fromIREventBar def = partT
  where
    partT (Part { part_sections = ss }) = 
          T.Part { T.part_sections = map sectionT ss }

    sectionT (Section { section_name = name
                      , section_bars = bs   })  = 
          T.Section { T.section_name = name
                    , T.section_events = concatMap barT bs }

    barT (Bar { bar_onset  = ot
              , bar_events = es })  = map (eventT ot) es

    eventT onsetb (Event o p d a)   = 
          let body = (genBodyFromEvent def) p a in T.Event (onsetb + o) d body

    eventT onsetb (Grace o p d)     = 
          let body = (genBodyFromGrace def) p in T.Event (onsetb + o) d body


--------------------------------------------------------------------------------

data GenEventBody2 pch anno body = GenEventBody2
    { genBodyFromEvent2       :: pch -> anno -> (body,body)
    , genBodyFromGrace2       :: pch -> (body,body)
    }


fromIREventBar2 :: GenEventBody2 pch anno body
                -> Part pch anno 
                -> T.Part Seconds () body
fromIREventBar2 def = partT
  where
    partT (Part { part_sections = ss }) = 
          T.Part { T.part_sections = map sectionT ss }

    sectionT (Section { section_name = name
                      , section_bars = bs   })  = 
          T.Section { T.section_name = name
                    , T.section_events = concatMap barT bs }

    barT (Bar { bar_onset  = ot
              , bar_events = es })  = concatMap (eventT ot) es

    eventT onsetb (Event o p d a)   = 
          let (body1,body2) = (genBodyFromEvent2 def) p a 
              t1            = onsetb + o
              t2            = t1 + d
          in [ T.Event t1 () body1, T.Event t2 () body2 ]

    eventT onsetb (Grace o p d)     = 
          let (body1,body2) = (genBodyFromGrace2 def) p 
              t1            = onsetb + o
              t2            = t1 + d
          in [ T.Event t1 () body1, T.Event t2 () body2 ]

