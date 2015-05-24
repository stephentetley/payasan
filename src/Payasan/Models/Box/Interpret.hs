{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Box.Interpret
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Box Notation
--
--------------------------------------------------------------------------------

module Payasan.Models.Box.Interpret
  (
    renderBoxPhrase
  , boxPhrase
  ) where


import Payasan.Models.Box.Base

import Payasan.Base
import Payasan.Base.Advance



renderBoxPhrase :: StrokeF -> TrackData -> BoxPhrase a -> Track
renderBoxPhrase fn cfg phz = renderAdvance cfg $ getBoxPhrase phz fn

-- | A beat in Box system is a quarter note
--
boxPhrase :: BoxPattern -> BoxPhrase ()
boxPhrase patt = BoxPhrase $ \tf ->
    quarter_note_length >>= \len ->
    mapM_ (renderElem tf len) $ extrBoxPattern patt 
    


-- | Note chordFw_ is probably wrong place 
renderElem :: StrokeF -> Seconds -> Elem -> Advance ()
renderElem sf len (N iden)    = event_ len (sf iden)
renderElem _  len (R)         = advanceCursor len


