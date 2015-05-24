{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Box.Base
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Box notation
--
--------------------------------------------------------------------------------

module Payasan.Models.Box.Base
  (

    StrokeF
  , BoxPhrase(..)
  , Elem(..)
  , Identifier(..)  

  , BoxPattern
  , makeBoxPattern
  , extrBoxPattern

  ) where


import Payasan.Base
import Payasan.Base.Advance
import Payasan.Base.Internal.Utils

import Control.Applicative
import Data.Data
import Data.Monoid




-- | This should probably be a fixed width event.
--
-- It is a percussion sound which is expected to have decayed
-- before the noteoff time.
--
type StrokeF = Identifier -> Event ()


--------------------------------------------------------------------------------
-- Low level


newtype BoxPhrase a = BoxPhrase { getBoxPhrase :: StrokeF -> Advance a } 


instance Monoid a => Monoid (BoxPhrase a) where
  mempty        = BoxPhrase $ \_    -> mempty
  a `mappend` b = BoxPhrase $ \tfun -> 
                    getBoxPhrase a tfun `mappend` getBoxPhrase b tfun


instance Functor BoxPhrase where
  fmap f ma = BoxPhrase $ \tfun -> fmap f $ getBoxPhrase ma tfun


instance Applicative BoxPhrase where
  pure a    = BoxPhrase $ \_    -> pure a
  mf <*> ma = BoxPhrase $ \tfun -> 
                getBoxPhrase mf tfun <*> getBoxPhrase ma tfun


instance Monad BoxPhrase where
  return a  = BoxPhrase $ \_    -> return a
  ma >>= k  = BoxPhrase $ \tfun -> 
                getBoxPhrase ma tfun >>= \a -> getBoxPhrase (k a) tfun


instance ContextM BoxPhrase where
  askCtx        = BoxPhrase $ \_    -> askCtx 
  asksCtx f     = BoxPhrase $ \_    -> asksCtx f
  localize f ma = BoxPhrase $ \tfun -> localize f $ getBoxPhrase ma tfun


--------------------------------------------------------------------------------
-- High level - symbolic


-- | Use an identifier to /help/ template haskell
--
-- 
data Elem = N Identifier
          | R 
  deriving (Data,Eq,Ord,Show,Typeable)



newtype Identifier = Identifier { getIdentifier :: String }
  deriving (Data,Eq,Ord,Show,Typeable)





--------------------------------------------------------------------------------
-- High level group type


-- need a concat list type that works well with Data.Data...
-- data.Sequence

newtype BoxPattern = BoxPattern { getBoxPattern :: CatList Elem }
  deriving (Data,Eq,Ord,Typeable)


instance Show BoxPattern where
  show = show . toListCat . getBoxPattern


instance Monoid BoxPattern where
  mempty        = BoxPattern emptyCat
  a `mappend` b = BoxPattern $ getBoxPattern a `appendCat` getBoxPattern b

makeBoxPattern :: [Elem] -> BoxPattern
makeBoxPattern = BoxPattern . fromListCat

extrBoxPattern :: BoxPattern -> [Elem]
extrBoxPattern = toListCat . getBoxPattern

