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


-- NOTE - there is no obligation to fix the type of Onset to
-- Seconds, although it is unlikely to be anything else. 

fromIREventBar :: Num ot => Part ot evt -> T.Part ot evt
fromIREventBar = partT


partT :: Num ot => Part ot evt -> T.Part ot evt
partT (Part ss)                     = 
    T.Part { T.part_sections = map sectionT ss }

sectionT :: Num ot => Section ot evt -> T.Section ot evt
sectionT (Section { section_name = name
                  , section_bars = bs   })  = 
    T.Section { T.section_name = name
              , T.section_events = concatMap barT bs
              }

barT :: Num ot => Bar ot evt -> [T.Event ot evt]
barT (Bar ot cs)                    = map (eventT ot) cs

eventT :: Num ot => ot -> Event ot evt -> T.Event ot evt
eventT onsetb (Event ot body)   = 
    T.Event { T.event_onset     = onsetb + ot
            , T.event_body      = body
            }