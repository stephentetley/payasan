{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.ChordProgression
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

module Payasan.Models.ChordProgression
  ( 

  -- * Payasan.Models.ChordProgression.Base
    ChordF
  , NoteF
  , ChordNotes
  , ChordPhrase
  , renderChordPhrase

  , ChordProgression
  , makeChordProgression
  , extrChordProgression
  , chordPhrase
  , makeChordF

  -- Re-export
  , Elem(..)

  ) where


import Payasan.Models.ChordProgression.Base
import Payasan.Symbolic.SimplePhrase

