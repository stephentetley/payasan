{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Melody.TopSyntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Top level syntax based on ABC notation.
--
--------------------------------------------------------------------------------

module Payasan.Models.Melody.AbcSyntax
  ( 

    AbcMotif(..)
  , Element(..)
  , ScorePitch(..)
  , PitchLetter(..)
  , Octave(..)
  , Accidental(..)
  , NoteLength(..)

  , fromAbcLike
  , toPitch
  , toBeat
  , defaultNoteLength
  , default_note_length

  ) where

import qualified Payasan.Models.Melody.Base as BASE

import Payasan.Base
import Payasan.Base.Names.Duration
import Payasan.Base.Names.Pitch

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Data.Data
import Data.Ratio



--------------------------------------------------------------------------------
-- Syntax


data AbcMotif = AbcMotif 
    { abc_time_sig      :: Maybe TimeSig
    , abc_elements      :: [Element]
    }
  deriving (Data,Eq,Ord,Show,Typeable)


data Element = Note ScorePitch NoteLength
             | Rest NoteLength
  deriving (Data,Eq,Ord,Show,Typeable)

data ScorePitch = ScorePitch Accidental PitchLetter Octave
  deriving (Data,Eq,Ord,Show,Typeable)

-- | Two octave range - upper and lower
data PitchLetter = CU | DU | EU | FU | GU | AU | BU 
                 | CL | DL | EL | FL | GL | AL | BL 
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

data Octave = DefaultOve
            | Pos Int
            | Neg Int
  deriving (Data,Eq,Ord,Show,Typeable)


data Accidental = DBL_FLAT | FLAT | NATURAL | SHARP | DBL_SHARP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


data NoteLength = DNL
                | Mult Int
                | Divd Int
  deriving (Data,Eq,Ord,Show,Typeable)



--------------------------------------------------------------------------------
-- Conversion

-- Note - carry around time signature in the ABC string...


fromAbcLike :: AbcMotif -> BASE.Melody
fromAbcLike (AbcMotif { abc_time_sig = mb_sig, abc_elements = xs}) = 
    BASE.makeMelody $ map (fromElement dnl) xs
  where
    dnl = defaultNoteLength $ maybe (4,4) id mb_sig


fromElement :: Beat -> Element -> BASE.Elem Pitch
fromElement dnl (Note p d) = BASE.N (toPitch p) (dnl * toBeat d)
fromElement dnl (Rest d)   = BASE.R (dnl * toBeat d)

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