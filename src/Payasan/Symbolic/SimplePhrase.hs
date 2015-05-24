{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Symbolic.SimplePhrase
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

module Payasan.Symbolic.SimplePhrase
  (

    InterpF
  , Phrase
  , renderPhrase
  , Elem(..)
  , Motif
  , makeMotif
  , extrMotif
  , phrase

  -- * Motif operations
  , mapNote
  , mapDuration
  , foldrDuration
  , beatSum

  ) where


import Payasan.Base
import Payasan.Base.Advance
import Payasan.Base.Internal.Utils

import Control.Applicative
import Data.Data
import qualified Data.Foldable as F
import Data.Monoid
import qualified Data.Traversable as T




-- | ...
type InterpF e = e -> Seconds -> Advance ()


--------------------------------------------------------------------------------
-- Low level


newtype Phrase e a = Phrase { getPhrase :: InterpF e -> Advance a }

instance Monoid a => Monoid (Phrase fn a) where
  mempty        = Phrase $ \_ -> mempty
  a `mappend` b = Phrase $ \r -> getPhrase a r `mappend` getPhrase b r

instance Functor (Phrase fn) where
  fmap f ma = Phrase $ \r -> fmap f $ getPhrase ma r

instance Applicative (Phrase fn) where
  pure a    = Phrase $ \_ -> pure a
  mf <*> ma = Phrase $ \r -> getPhrase mf r <*> getPhrase ma r

instance Monad (Phrase fn) where
  return a  = Phrase $ \_    -> return a
  ma >>= k  = Phrase $ \r -> 
                getPhrase ma r >>= \a -> getPhrase (k a) r

instance ContextM (Phrase fn) where
  askCtx        = Phrase $ \_ -> askCtx 
  asksCtx f     = Phrase $ \_ -> asksCtx f
  localize f ma = Phrase $ \r -> localize f $ getPhrase ma r


renderPhrase :: InterpF e -> TrackData -> Phrase e a -> Track
renderPhrase fn cfg phz = renderAdvance cfg $ getPhrase phz fn




--------------------------------------------------------------------------------
-- High level - symbolic


-- | Note and Rest
-- 
-- Note is parametric on pitch, which might be e.g. a drum name 
-- or a chord pitch set rather than a simply a chromatic pitch.
--
data Elem e = N e Beat
            | R Beat
  deriving (Data,Eq,Ord,Show,Typeable)


instance Functor Elem where
  fmap f (N a drn)  = N (f a) drn
  fmap _ (R drn)    = R drn

instance F.Foldable Elem where
  foldMap f (N a _) = f a
  foldMap _ (R _)   = mempty

instance T.Traversable Elem where
  traverse f (N a drn)  = (\x -> N x drn) <$> f a
  traverse _ (R drn)    = pure $ R drn


--------------------------------------------------------------------------------
-- High level group type


-- need a concat list type that works well with Data.Data...
-- data.Sequence

newtype Motif pch = Motif { getMotif :: CatList (Elem pch) }
  deriving (Data,Eq,Ord,Typeable)

instance Show e => Show (Motif e) where
  show = show . toListCat . getMotif

instance Monoid (Motif e) where
  mempty        = Motif $ emptyCat
  a `mappend` b = Motif $ getMotif a `appendCat` getMotif b

instance Functor Motif where
  fmap f = Motif . fmap (fmap f) . getMotif

instance F.Foldable Motif where
  foldMap f (Motif xs) = F.foldMap (F.foldMap f) xs

instance T.Traversable Motif where
  traverse f (Motif xs) = Motif <$> T.traverse (T.traverse f) xs

makeMotif :: [Elem e] -> Motif e
makeMotif = Motif . fromListCat

extrMotif :: Motif e -> [Elem e]
extrMotif = toListCat . getMotif


-- | A beat in Chord system is a quarter note
--
phrase :: Motif e -> Phrase e ()
phrase mo = Phrase $ \tf -> mapM_ (renderElem tf) $ extrMotif mo
  


renderElem :: InterpF e -> Elem e -> Advance ()
renderElem sf (N e d)    = durationInSeconds d >>= \len -> sf e len
renderElem _  (R d)      = durationInSeconds d >>= advanceCursor


--------------------------------------------------------------------------------
-- Operations



mapNote :: (e1 -> e2) -> Motif e1 -> Motif e2
mapNote f (Motif xs) = Motif $ fmap f2 xs
  where
    f2 (N p d) = N (f p) d
    f2 (R d)   = R d

mapDuration :: (Beat -> Beat) -> Motif e -> Motif e
mapDuration f (Motif xs) = Motif $ fmap f2 xs
  where
    f2 (N e d) = N e (f d)
    f2 (R d)   = R (f d)


foldrDuration :: (Beat -> a -> a) -> a -> Motif e -> a
foldrDuration f zero (Motif xs) = F.foldr f2 zero xs
  where
    f2 (N _ d) ac = f d ac
    f2 (R d)   ac = f d ac


beatSum :: Motif e -> Beat
beatSum = foldrDuration (+) 0