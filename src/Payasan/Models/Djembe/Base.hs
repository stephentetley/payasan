{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Djembe.Base
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Djembe
--
--------------------------------------------------------------------------------

module Payasan.Models.Djembe.Base
  (

    StrokeF
  , DjembePhrase(..)
  , Note(..)
  , Accent(..)
  , Identifier(..)  
  , Wrapper(..)

  , DjembePattern
  , makeDjembePattern
  , extrDjembePattern

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
type StrokeF = Accent -> Identifier -> Event ()


--------------------------------------------------------------------------------
-- Low level


newtype DjembePhrase a = DjembePhrase { getDjembePhrase :: StrokeF -> Advance a } 


instance Monoid a => Monoid (DjembePhrase a) where
  mempty        = DjembePhrase $ \_    -> mempty
  a `mappend` b = DjembePhrase $ \tfun -> 
                    getDjembePhrase a tfun `mappend` getDjembePhrase b tfun


instance Functor DjembePhrase where
  fmap f ma = DjembePhrase $ \tfun -> fmap f $ getDjembePhrase ma tfun


instance Applicative DjembePhrase where
  pure a    = DjembePhrase $ \_    -> pure a
  mf <*> ma = DjembePhrase $ \tfun -> 
                getDjembePhrase mf tfun <*> getDjembePhrase ma tfun


instance Monad DjembePhrase where
  return a  = DjembePhrase $ \_    -> return a
  ma >>= k  = DjembePhrase $ \tfun -> 
                getDjembePhrase ma tfun >>= \a -> getDjembePhrase (k a) tfun


instance ContextM DjembePhrase where
  askCtx        = DjembePhrase $ \_    -> askCtx 
  asksCtx f     = DjembePhrase $ \_    -> asksCtx f
  localize f ma = DjembePhrase $ \tfun -> localize f $ getDjembePhrase ma tfun


--------------------------------------------------------------------------------
-- High level - symbolic


-- | Use an identifier to /help/ template haskell
--
-- 
data Note = N Accent Identifier
          | C Accent [Identifier]
          | R 
  deriving (Data,Eq,Ord,Show,Typeable)


-- | Seems like there is only a two element alphabet for Accent.
-- Can be fixed, not worth being parametric.
--
data Accent = ACCENT | NO_ACCENT
  deriving (Data,Eq,Ord,Show,Typeable)


newtype Identifier = Identifier { getIdentifier :: String }
  deriving (Data,Eq,Ord,Show,Typeable)




-- Use advance for underlying structure...
--
-- swing          :: Note -> Output
-- flam           :: Identifier -> Note -> Output
-- duplet         :: Note -> Note -> Output
-- triplet        :: Note -> Note -> Note -> Output
--
-- Or...
--
-- If we use the analogies:
--
-- Notes   = characters
-- Wrappers = words
-- 
-- We have a small enough *fixed set* of wrappers that we can 
-- use a data type for /words/ rather than render them directly.
-- (We have already decided they are mutually exclusive...)
--
-- Aka - deep or shallow embeddings.
--


data Wrapper = One     Note
             | Swing   Note 
             | Flam    Note Note
             | Duplet  Note Note
             | Triplet Note Note Note
  deriving (Data,Eq,Ord,Show,Typeable)


-- type WrapperList = [Wrapper]


--------------------------------------------------------------------------------
-- High level group type


-- need a concat list type that works well with Data.Data...
-- data.Sequence

newtype DjembePattern = DjembePattern { getDjembePattern :: CatList Wrapper }
  deriving (Data,Eq,Ord,Typeable)


instance Show DjembePattern where
  show = show . toListCat . getDjembePattern


instance Monoid DjembePattern where
  mempty        = DjembePattern emptyCat
  a `mappend` b = DjembePattern $ getDjembePattern a `appendCat` getDjembePattern b

makeDjembePattern :: [Wrapper] -> DjembePattern
makeDjembePattern = DjembePattern . fromListCat

extrDjembePattern :: DjembePattern -> [Wrapper]
extrDjembePattern = toListCat . getDjembePattern

