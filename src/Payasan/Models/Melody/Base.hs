{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Melody.Base
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Sequences of notes / rests.
--
--------------------------------------------------------------------------------

module Payasan.Models.Melody.Base
  (

    Element
  , MelodyF
  , NoteF
  , MelodyPhrase
  , renderMelodyPhrase

  , Melody
  , makeMelody
  , extrMelody
  , melodyPhrase
  , makeMelodyF

  , mapNote
  , mapDuration

  -- * Re-exports
  , Elem(..) 

  ) where


import qualified Payasan.Symbolic.SimplePhrase as SP
import Payasan.Symbolic.SimplePhrase ( Elem(..) )
 
import Payasan.Base
import Payasan.Base.Advance


import Control.Applicative
import Data.Data
import Data.Monoid


type Element = SP.Elem Pitch
type NoteF  = Pitch -> Event ()
type MelodyF = SP.InterpF Pitch

--------------------------------------------------------------------------------
-- Low level




newtype MelodyPhrase a = MelodyPhrase { getMelodyPhrase :: SP.Phrase Pitch a } 

instance Monoid a => Monoid (MelodyPhrase a) where
  mempty        = MelodyPhrase $ mempty
  a `mappend` b = MelodyPhrase $ getMelodyPhrase a `mappend` getMelodyPhrase b

instance Functor MelodyPhrase where
  fmap f ma = MelodyPhrase $ fmap f $ getMelodyPhrase ma

instance Applicative MelodyPhrase where
  pure a    = MelodyPhrase $ pure a
  mf <*> ma = MelodyPhrase $ getMelodyPhrase mf <*> getMelodyPhrase ma

instance Monad MelodyPhrase where
  return a  = MelodyPhrase $ return a
  ma >>= k  = MelodyPhrase $ getMelodyPhrase ma >>= \a -> getMelodyPhrase (k a)

instance ContextM MelodyPhrase where
  askCtx        = MelodyPhrase $ askCtx 
  asksCtx f     = MelodyPhrase $ asksCtx f
  localize f ma = MelodyPhrase $ localize f $ getMelodyPhrase ma



renderMelodyPhrase :: MelodyF -> TrackData -> MelodyPhrase a -> Track
renderMelodyPhrase fn cfg phz = SP.renderPhrase fn cfg $ getMelodyPhrase phz




--------------------------------------------------------------------------------
-- High level group type



-- Should melodies carry time signature to be compatible with Abc?
-- Problemmatic for concat...
-- Time sig can be /captured/ by concrete syntax.

data Melody = Melody { getMelody :: SP.Motif Pitch }
  deriving (Data,Eq,Ord,Show,Typeable)


instance Monoid Melody where
  mempty        = Melody mempty
  a `mappend` b = Melody $ getMelody a `mappend` getMelody b


makeMelody :: [Element] -> Melody
makeMelody = Melody . SP.makeMotif

extrMelody :: Melody -> [Element]
extrMelody = SP.extrMotif . getMelody


melodyPhrase :: Melody -> MelodyPhrase ()
melodyPhrase e = MelodyPhrase $ SP.phrase $ getMelody e


makeMelodyF :: NoteF -> MelodyF
makeMelodyF mf = \pch drn -> event_ drn (mf pch)


mapNote :: (Pitch -> Pitch) -> Melody -> Melody
mapNote f = Melody . SP.mapNote f . getMelody

mapDuration :: (Beat -> Beat) -> Melody -> Melody
mapDuration f = Melody . SP.mapDuration f . getMelody
