{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.Syntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist segmented into bars, with notes, rests, 
-- chords, grace notes and triplets.
--
-- Concrete syntax following ABC.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABC.Syntax
  ( 
    module Payasan.Base.Internal.CommonSyntax

  , ABCPhrase
  , ABCBar
  , ABCCtxElement
  , ABCElement
  , ABCNote

  , Phrase(..)
  , Bar(..)
  , CtxElement(..)
  , Element(..)
  , Note(..)
  , Accidental(..)
  , PitchLetter(..)
  , Octave(..)
  , Pitch(..)
  , NoteLength(..)

  ) where

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax

import Data.Data

--------------------------------------------------------------------------------
-- Syntax


type ABCPhrase          = Phrase      Pitch NoteLength
type ABCBar             = Bar         Pitch NoteLength
type ABCCtxElement      = CtxElement  Pitch NoteLength
type ABCElement         = Element     Pitch NoteLength
type ABCNote            = Note        Pitch NoteLength



data Pitch = Pitch Accidental PitchLetter Octave
  deriving (Data,Eq,Ord,Show,Typeable)


data Accidental = NO_ACCIDENTAL | DBL_FLAT | FLAT | NATURAL | SHARP | DBL_SHARP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

-- | Two octave range - upper and lower
data PitchLetter = CU | DU | EU | FU | GU | AU | BU 
                 | CL | DL | EL | FL | GL | AL | BL 
  deriving (Data,Enum,Eq,Ord,Show,Typeable)


data Octave = OveDefault
            | OveRaised   Int
            | OveLowered  Int
  deriving (Data,Eq,Ord,Show,Typeable)



-- | does this need @Frac Int Int@ ?
--
data NoteLength = DNL
                | Mult Int
                | Divd Int
                | Frac Int Int
  deriving (Data,Eq,Ord,Show,Typeable)





