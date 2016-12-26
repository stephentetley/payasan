{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventFlat.FromIREventBeam
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translate IREventBeam to IREventFlat.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IREventFlat.FromIREventBeam
  ( 
    fromIREventBeam
  ) where

import Payasan.PSC.Repr.IREventBeam.Syntax
import qualified Payasan.PSC.Repr.IREventFlat.Syntax as T


-- NOTE - there is no obligation to fix the type of Onset to
-- Seconds, although it is unlikely to be anything else. 

fromIREventBeam :: Num ot => Part ot evt -> T.Part ot evt
fromIREventBeam = partT


partT :: Num ot => Part ot evt -> T.Part ot evt
partT (Part ss)                     = 
    T.Part { T.part_events = concatMap sectionT ss }

sectionT :: Num ot => Section ot evt -> [T.Event ot evt]
sectionT (Section { section_bars = ss })      = concatMap barT ss

barT :: Num ot => Bar ot evt -> [T.Event ot evt]
barT (Bar ot cs)                    = map (eventT ot) cs

eventT :: Num ot => ot -> Event ot evt -> T.Event ot evt
eventT onsetb (Event ot body)   = 
    T.Event { T.event_onset     = onsetb + ot
            , T.event_body      = body
            }