{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IREventBeamToIREventFlat
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

module Payasan.PSC.Repr.IREventBeamToIREventFlat
  ( 

    transIREventBeamToIREventFlat

  ) where

import Payasan.PSC.Repr.IREventBeam.Syntax
import qualified Payasan.PSC.Repr.IREventFlat.Syntax as T


transIREventBeamToIREventFlat :: Num ot => Part ot drn note -> T.Part ot drn note
transIREventBeamToIREventFlat = partT


partT :: Num ot => Part ot drn note -> T.Part ot drn note
partT (Part bs)                         = 
    T.Part { T.part_events = concat $ map barT bs }

barT :: Num ot => Bar ot drn note -> [T.Event ot drn note]
barT (Bar ot cs) = map (eventT ot) cs

eventT :: Num ot => ot -> Event ot drn note -> T.Event ot drn note
eventT onsetb (Event dt drn note)    = 
    T.Event { T.event_onset     = onsetb + dt
            , T.event_duration  = drn
            , T.event_note      = note 
            }