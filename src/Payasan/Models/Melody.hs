{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Melody
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Chord Progressions
--
--------------------------------------------------------------------------------

module Payasan.Models.Melody
  ( 

  -- * Payasan.Models.Melody.Base
    MelodyF
  , NoteF
  , MelodyPhrase
  , renderMelodyPhrase

  , Melody
  , makeMelody
  , extrMelody
  , melodyPhrase
  , makeMelodyF

  , melody

  -- Re-export
  , Elem(..)

  ) where


import Payasan.Models.Melody.Base
import Payasan.Models.Melody.Parser



