{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Metronome.Interpret
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- // Djembe - LMMS use sound font player and bank 128.
--
--------------------------------------------------------------------------------

module Payasan.Models.Metronome.Interpret
  ( 
    metroPhrase
  , renderMetroPhrase
  ) where


import Payasan.Models.Metronome.Base

import Payasan.Base
import Payasan.Base.Advance

import Control.Applicative


-- | Can probably have a version with default TickF...
--
renderMetroPhrase :: TickF -> TrackData -> MetroPhrase a -> Track
renderMetroPhrase fn cfg phz = renderAdvance cfg $ getMetroPhrase phz fn


metroPhrase :: Metronome -> MetroPhrase ()
metroPhrase patt = MetroPhrase $ \tf ->
    mapM_ (renderBar tf) $ extrMetronome patt


getTickLength :: Beat -> Advance Seconds
getTickLength a = fn <$> quarter_note_length
  where 
    fn s = s * realToFrac a


renderBar :: TickF -> Bar -> Advance ()
renderBar tf (Bar { bar_tick_is  = tick_is
                  , bar_ticks    = ts 
                  }) = 
   do { len <- getTickLength tick_is
      ; mapM_ (render1 tf len) ts
      }

render1 :: TickF -> Seconds -> Tick -> Advance ()
render1 tf drn tick = event_ drn (tf tick)

