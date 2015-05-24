{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.ChordProgression.Base
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Sequences of chords / chord progressions.
--
--------------------------------------------------------------------------------

module Payasan.Models.ChordProgression.Base
  (

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

  ) where


import Payasan.Symbolic.SimplePhrase

import Payasan.Base
import Payasan.Base.Advance


import Control.Applicative
import Data.Data
import Data.Monoid



type NoteF  = Pitch -> Event ()
type ChordF = InterpF ChordNotes

--------------------------------------------------------------------------------
-- Low level


type ChordNotes = [Pitch]


newtype ChordPhrase a = ChordPhrase { getChordPhrase :: Phrase ChordNotes a  } 

instance Monoid a => Monoid (ChordPhrase a) where
  mempty        = ChordPhrase $ mempty
  a `mappend` b = ChordPhrase $ getChordPhrase a `mappend` getChordPhrase b

instance Functor ChordPhrase where
  fmap f ma = ChordPhrase $ fmap f $ getChordPhrase ma

instance Applicative ChordPhrase where
  pure a    = ChordPhrase $ pure a
  mf <*> ma = ChordPhrase $ getChordPhrase mf <*> getChordPhrase ma

instance Monad ChordPhrase where
  return a  = ChordPhrase $ return a
  ma >>= k  = ChordPhrase $ getChordPhrase ma >>= \a -> getChordPhrase (k a)

instance ContextM ChordPhrase where
  askCtx        = ChordPhrase $ askCtx 
  asksCtx f     = ChordPhrase $ asksCtx f
  localize f ma = ChordPhrase $ localize f $ getChordPhrase ma



renderChordPhrase :: InterpF ChordNotes -> TrackData -> ChordPhrase a -> Track
renderChordPhrase fn cfg phz = renderPhrase fn cfg $ getChordPhrase phz




--------------------------------------------------------------------------------
-- High level group type





newtype ChordProgression = ChordProgression { getChordProgression :: Motif ChordNotes }
  deriving (Data,Eq,Ord,Show,Typeable)


instance Monoid ChordProgression where
  mempty        = ChordProgression mempty
  a `mappend` b = ChordProgression $ getChordProgression a `mappend` getChordProgression b


makeChordProgression :: [Elem ChordNotes] -> ChordProgression
makeChordProgression = ChordProgression . makeMotif

extrChordProgression :: ChordProgression -> [Elem ChordNotes]
extrChordProgression = extrMotif . getChordProgression


chordPhrase :: ChordProgression -> ChordPhrase ()
chordPhrase e = ChordPhrase $ phrase $ getChordProgression e


makeChordF :: NoteF -> ChordF
makeChordF mf = \ps drn -> chord_ drn ps mf
