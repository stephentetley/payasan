{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.LilyPond.Common
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist segmented into bars, with notes, rests, 
-- chords, grace notes and triplets.
--
-- Adapted syntax following Lilypond.
--
--------------------------------------------------------------------------------

module Payasan.PSC.LilyPond.Common
  ( 


    LyTupletSpec(..)
  , LyPitch(..)
  , Accidental(..)
  , PitchLetter(..)
  , Octave(..)
  , LyNoteLength(..)

  , middle_c


  -- * Conversion
  , toPitchAbs
  , fromPitchAbs

  , toPitchName
  , fromPitchName

  , toPitchLetter
  , fromPitchLetter
  , toAlteration
  , fromAlteration

  , toPitchRel
  , fromPitchRel

  ) where

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as PCH


import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data




-- | LilyPond has a simpler Tuplet spec than ABC which we 
-- expand during parsing.
-- 
data LyTupletSpec = LyTupletSpec Int Int
  deriving (Data,Eq,Show,Typeable)


data LyPitch = LyPitch PitchLetter Accidental Octave
  deriving (Data,Eq,Ord,Show,Typeable)


data Accidental = NO_ACCIDENTAL | DBL_FLAT | FLAT | NATURAL | SHARP | DBL_SHARP
  deriving (Data,Enum,Eq,Ord,Show,Typeable)

-- | One octave range - just lower
data PitchLetter = C | D | E | F | G | A | B
  deriving (Data,Enum,Eq,Ord,Show,Typeable)



data Octave = OveDefault
            | OveRaised   Int
            | OveLowered  Int
  deriving (Data,Eq,Ord,Show,Typeable)


data LyNoteLength = DrnDefault
                  | DrnExplicit Duration
  deriving (Data,Eq,Ord,Show,Typeable)



middle_c :: LyPitch
middle_c = LyPitch C NO_ACCIDENTAL (OveRaised 1)


--------------------------------------------------------------------------------
-- Conversion


-- | CU = middle C (octave 4)
--
toPitchAbs :: LyPitch -> PCH.Pitch
toPitchAbs (LyPitch l a om) =
    let lbl = PCH.PitchName (toPitchLetter l) (toAlteration a) 
    in PCH.Pitch lbl (toOctaveAbs om)



fromPitchAbs :: PCH.Pitch -> LyPitch
fromPitchAbs (PCH.Pitch lbl o) = 
    let (l,a) = fromPitchName lbl
    in LyPitch l a (fromOctaveAbs o)



toPitchName :: PitchLetter -> Accidental -> PCH.PitchName
toPitchName l a = PCH.PitchName (toPitchLetter l) (toAlteration a)

fromPitchName :: PCH.PitchName -> (PitchLetter,Accidental)
fromPitchName (PCH.PitchName p a) = (fromPitchLetter p, fromAlteration a)


-- | Middle c is 4. In LilyPond this is C raised 1
--
toOctaveAbs :: Octave -> PCH.Octave
toOctaveAbs (OveDefault)    = 3
toOctaveAbs (OveRaised n)   = 3 + n
toOctaveAbs (OveLowered n)  = 3 - n


fromOctaveAbs :: PCH.Octave -> Octave
fromOctaveAbs n 
    | n >  3                = OveRaised (n-3)
    | n == 3                = OveDefault
    | otherwise             = OveLowered (3-n)

toPitchLetter :: PitchLetter -> PCH.PitchLetter
toPitchLetter C = PCH.C
toPitchLetter D = PCH.D
toPitchLetter E = PCH.E
toPitchLetter F = PCH.F
toPitchLetter G = PCH.G
toPitchLetter A = PCH.A
toPitchLetter B = PCH.B

fromPitchLetter :: PCH.PitchLetter -> PitchLetter
fromPitchLetter (PCH.C) = C
fromPitchLetter (PCH.D) = D
fromPitchLetter (PCH.E) = E
fromPitchLetter (PCH.F) = F
fromPitchLetter (PCH.G) = G
fromPitchLetter (PCH.A) = A
fromPitchLetter (PCH.B) = B


toAlteration :: Accidental -> PCH.Alteration
toAlteration NO_ACCIDENTAL      = PCH.NAT
toAlteration DBL_FLAT           = PCH.DBL_FLAT
toAlteration FLAT               = PCH.FLAT
toAlteration NATURAL            = PCH.NAT
toAlteration SHARP              = PCH.SHARP
toAlteration DBL_SHARP          = PCH.DBL_SHARP

fromAlteration :: PCH.Alteration -> Accidental
fromAlteration PCH.DBL_FLAT     = DBL_FLAT
fromAlteration PCH.FLAT         = FLAT
fromAlteration PCH.NAT          = NO_ACCIDENTAL
fromAlteration PCH.SHARP        = SHARP
fromAlteration PCH.DBL_SHARP    = DBL_SHARP


makeOctaveModifier :: Int -> Octave
makeOctaveModifier i 
    | i == 0    = OveDefault
    | i >  0    = OveRaised i
    | otherwise = OveLowered (abs i)


fromPitchRel :: PCH.Pitch -> PCH.Pitch -> LyPitch
fromPitchRel p_old p1 = 
    let om    = makeOctaveModifier $ PCH.lyOctaveDistance p_old p1
        (l,a) = fromPitchName $ PCH.pitch_name p1        
    in LyPitch l a om




-- | In relative mode, the octave modifier is oblivious
-- to the initial calculation of the pitch /within a fourth/.
--
toPitchRel :: PCH.Pitch -> LyPitch -> PCH.Pitch
toPitchRel p0 (LyPitch l a om) = octaveAdjust root om
  where
    lbl   = toPitchName l a
    psame = PCH.Pitch lbl (PCH.pitch_octave p0)
    pup   = octaveAdjust psame $ OveRaised 1
    pdown = octaveAdjust psame $ OveLowered 1
    root  = withinFourth p0 psame pup pdown

withinFourth :: PCH.Pitch -> PCH.Pitch -> PCH.Pitch -> PCH.Pitch -> PCH.Pitch
withinFourth p0 a b c
    | PCH.arithmeticDistance p0 a <= 4  = a
    | PCH.arithmeticDistance p0 b <= 4  = b
    | otherwise                         = c



octaveAdjust :: PCH.Pitch -> Octave -> PCH.Pitch
octaveAdjust pch@(PCH.Pitch lbl o) om = case om of
    OveDefault      -> pch
    OveRaised i     -> PCH.Pitch lbl (o+i)
    OveLowered i    -> PCH.Pitch lbl (o-i)




--------------------------------------------------------------------------------
-- Pretty instances are for debugging and do not correspond 
-- to valid LilyPond

instance Pretty LyPitch where 
  pPrint (LyPitch l a om)       = pPrint l <> pPrint a <> pPrint om

instance Pretty Accidental where
  pPrint NO_ACCIDENTAL          = empty
  pPrint DBL_FLAT               = text "eses"
  pPrint FLAT                   = text "es"
  pPrint NATURAL                = text "nat"
  pPrint SHARP                  = text "is"
  pPrint DBL_SHARP              = text "isis"

instance Pretty PitchLetter where
  pPrint C                      = char 'c'
  pPrint D                      = char 'd'
  pPrint E                      = char 'e'
  pPrint F                      = char 'f'
  pPrint G                      = char 'g'
  pPrint A                      = char 'a'
  pPrint B                      = char 'b'

instance Pretty Octave where
  pPrint (OveDefault)           = empty
  pPrint (OveRaised i)          = text (replicate i '\'')
  pPrint (OveLowered i)         = text (replicate i ',')


instance Pretty LyNoteLength where
  pPrint (DrnDefault)           = char '.'
  pPrint (DrnExplicit d)        = pPrint d

