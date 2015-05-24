{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Metronome.Base
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generate click track (metronome) output.
--
-- Two beats are supported - accented and unaccented, difference 
-- is due to _pitch_ and on-off velocity (not volume).
--
--------------------------------------------------------------------------------

module Payasan.Models.Metronome.Base
  (
    TickF
  , MetroPhrase(..)
  , Tick(..)  
  , Bar(..)
  , Metronome(..)
  , extrMetronome  
  ) where



import Payasan.Base
import Payasan.Base.Advance
import Payasan.Base.Internal.Utils

import Control.Applicative
import Data.Data
import Data.Monoid


-- | This should always be a fixed width event.
--
-- It is just a tick.
--
type TickF = Tick -> Event ()


--------------------------------------------------------------------------------
-- Low level


newtype MetroPhrase a = MetroPhrase { getMetroPhrase :: TickF -> Advance a } 


instance Monoid a => Monoid (MetroPhrase a) where
  mempty        = MetroPhrase $ \_    -> mempty
  a `mappend` b = MetroPhrase $ \tfun -> 
                    getMetroPhrase a tfun `mappend` getMetroPhrase b tfun


instance Functor MetroPhrase where
  fmap f ma = MetroPhrase $ \tfun -> fmap f $ getMetroPhrase ma tfun


instance Applicative MetroPhrase where
  pure a    = MetroPhrase $ \_    -> pure a
  mf <*> ma = MetroPhrase $ \tfun -> 
                getMetroPhrase mf tfun <*> getMetroPhrase ma tfun


instance Monad MetroPhrase where
  return a  = MetroPhrase $ \_    -> return a
  ma >>= k  = MetroPhrase $ \tfun -> 
                getMetroPhrase ma tfun >>= \a -> getMetroPhrase (k a) tfun


instance ContextM MetroPhrase where
  askCtx        = MetroPhrase $ \_    -> askCtx 
  asksCtx f     = MetroPhrase $ \_    -> asksCtx f
  localize f ma = MetroPhrase $ \tfun -> localize f $ getMetroPhrase ma tfun


--------------------------------------------------------------------------------
-- Symbolic level

data Tick = TICK | STRONG_TICK
  deriving (Data,Eq,Ord,Show,Typeable)
          


-- Should BPM be associated with bar (permitting tempo change)?
--
data Bar = Bar 
    { bar_tick_is  :: Beat
    , bar_ticks    :: [Tick] 
    }
  deriving (Data,Eq,Show,Typeable)



newtype Metronome = Metronome { getMetronome :: CatList Bar } 
  deriving (Data,Eq,Typeable)


instance Show Metronome where
  show = show . toListCat . getMetronome


instance Monoid Metronome where
  mempty        = Metronome emptyCat
  a `mappend` b = Metronome $ getMetronome a `appendCat` getMetronome b


extrMetronome :: Metronome -> [Bar]
extrMetronome = toListCat . getMetronome



