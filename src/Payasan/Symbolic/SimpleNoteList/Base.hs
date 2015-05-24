{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Symbolic.SimpleNoteList.Base
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Simple Note List
--
--------------------------------------------------------------------------------

module Payasan.Symbolic.SimpleNoteList.Base
  ( 

    NoteF
  , Phrase(..)
  , Accidental(..)
  , PitchLetter(..)
  , Octave(..)
  , ScorePitch(..)
  , NoteLength(..)
  , Element(..)
  , Motif
  , makeMotif
  , extrMotif

  , toPitch
  , toBeat
  , defaultNoteLength
  , default_note_length

  ) where

import Payasan.Base
import Payasan.Base.Advance
import Payasan.Base.Names.Duration
import Payasan.Base.Names.Pitch
import Payasan.Base.Internal.Utils

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Data.Data
import Data.Monoid
import Data.Ratio


type NoteF = Pitch -> Event ()

--------------------------------------------------------------------------------
-- Low level


newtype Phrase a = Phrase { getPhrase :: NoteF -> Advance a } 


instance Monoid a => Monoid (Phrase a) where
  mempty        = Phrase $ \_    -> mempty
  a `mappend` b = Phrase $ \tfun -> 
                    getPhrase a tfun `mappend` getPhrase b tfun


instance Functor Phrase where
  fmap f ma = Phrase $ \tfun -> fmap f $ getPhrase ma tfun


instance Applicative Phrase where
  pure a    = Phrase $ \_    -> pure a
  mf <*> ma = Phrase $ \tfun -> 
                getPhrase mf tfun <*> getPhrase ma tfun


instance Monad Phrase where
  return a  = Phrase $ \_    -> return a
  ma >>= k  = Phrase $ \tfun -> 
                getPhrase ma tfun >>= \a -> getPhrase (k a) tfun


instance ContextM Phrase where
  askCtx        = Phrase $ \_    -> askCtx 
  asksCtx f     = Phrase $ \_    -> asksCtx f
  localize f ma = Phrase $ \tfun -> localize f $ getPhrase ma tfun

--------------------------------------------------------------------------------
-- Syntax

data Accidental = DBL_FLAT | FLAT | NATURAL | SHARP | DBL_SHARP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

-- | Two octave range - upper and lower
data PitchLetter = CU | DU | EU | FU | GU | AU | BU 
                 | CL | DL | EL | FL | GL | AL | BL 
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


data Octave = DefaultOve
            | Pos Int
            | Neg Int
  deriving (Data,Eq,Ord,Show,Typeable)


data ScorePitch = ScorePitch Accidental PitchLetter Octave
  deriving (Data,Eq,Ord,Show,Typeable)

data NoteLength = DNL
                | Mult Int
                | Divd Int
  deriving (Data,Eq,Ord,Show,Typeable)


data Element = Note ScorePitch NoteLength
             | Rest NoteLength
  deriving (Data,Eq,Ord,Show,Typeable)

-- type Score = [Element]



--------------------------------------------------------------------------------
-- High level group type


-- need a concat list type that works well with Data.Data...

newtype Motif = Motif { getMotif :: CatList Element }
  deriving (Data,Eq,Ord,Typeable)


instance Show Motif where
  show = show . toListCat . getMotif


instance Monoid Motif where
  mempty        = Motif emptyCat
  a `mappend` b = Motif $ getMotif a `appendCat` getMotif b

makeMotif :: [Element] -> Motif
makeMotif = Motif . fromListCat

extrMotif :: Motif -> [Element]
extrMotif = toListCat . getMotif


--------------------------------------------------------------------------------
-- Conversion

toPitch :: ScorePitch -> Pitch
toPitch (ScorePitch acc pl ove) = accidentalF acc $ oveF ove $ fromPL pl

fromPL :: PitchLetter -> Pitch
fromPL CU = middle_c
fromPL DU = d_4
fromPL EU = e_4
fromPL FU = f_4
fromPL GU = g_4
fromPL AU = a_4
fromPL BU = b_4
fromPL CL = c_5
fromPL DL = d_5
fromPL EL = e_5
fromPL FL = f_5
fromPL GL = g_5
fromPL AL = a_5
fromPL BL = b_5

accidentalF :: Accidental -> Pitch -> Pitch
accidentalF DBL_FLAT    = flatten . flatten
accidentalF FLAT        = flatten
accidentalF DBL_SHARP   = sharpen . sharpen
accidentalF SHARP       = sharpen
accidentalF NATURAL     = id

oveF :: Octave -> Pitch -> Pitch
oveF (Pos n)      p = p .+^ (fromIntegral $ n * 12)
oveF (Neg n)      p = p .-^ (fromIntegral $ n * 12)
oveF (DefaultOve) p = p



toBeat :: NoteLength -> Beat
toBeat (DNL)    = 1
toBeat (Mult i) = fromIntegral i
toBeat (Divd i) = fromIntegral i


defaultNoteLength :: TimeSig -> Beat
defaultNoteLength (n,d) = if frac < (3%4) then dsixteenth else deighth
   where
     frac:: Rational 
     frac = (fromIntegral n) % fromIntegral d 
                          

default_note_length :: (Monad m, ContextM m) => m Beat
default_note_length = defaultNoteLength <$> asksCtx ctx_time_sig