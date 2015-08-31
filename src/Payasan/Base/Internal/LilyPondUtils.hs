{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPondUtils
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Helpers for LilyPond output (pretty printers) and conversion.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPondUtils
  ( 


  -- * Conversion
    toPitchAbs
  , fromPitchAbs

  , toNoteLabel
  , fromNoteLabel

  , toPitchLetter
  , fromPitchLetter
  , toAlteration
  , fromAlteration

  , toPitchRel

  -- * Output

 
  ) where

import Payasan.Base.Internal.LilyPondSyntax

import qualified Payasan.Base.Pitch as P



--------------------------------------------------------------------------------
-- Conversion


-- | CU = middle C (octave 4)
--
toPitchAbs :: Pitch -> P.Pitch
toPitchAbs (Pitch l a om) =
    let lbl = P.NoteLabel (toPitchLetter l) (toAlteration a) 
    in P.Pitch lbl (toOctaveAbs om)



fromPitchAbs :: P.Pitch -> Pitch
fromPitchAbs (P.Pitch lbl o) = 
    let (l,a) = fromNoteLabel lbl
    in Pitch l a (fromOctaveAbs o)



toNoteLabel :: PitchLetter -> Accidental -> P.NoteLabel
toNoteLabel l a = P.NoteLabel (toPitchLetter l) (toAlteration a)

fromNoteLabel :: P.NoteLabel -> (PitchLetter,Accidental)
fromNoteLabel (P.NoteLabel p a) = (fromPitchLetter p, fromAlteration a)


-- | Middle c is 4. In LilyPond this is C raised 1
--
toOctaveAbs :: Octave -> P.Octave
toOctaveAbs (OveDefault)    = 3
toOctaveAbs (OveRaised n)   = 3 + n
toOctaveAbs (OveLowered n)  = 3 - n


fromOctaveAbs :: P.Octave -> Octave
fromOctaveAbs n 
    | n >  3                = OveRaised (n-3)
    | n == 3                = OveDefault
    | otherwise             = OveLowered (3-n)

toPitchLetter :: PitchLetter -> P.PitchLetter
toPitchLetter CL = P.C
toPitchLetter DL = P.D
toPitchLetter EL = P.E
toPitchLetter FL = P.F
toPitchLetter GL = P.G
toPitchLetter AL = P.A
toPitchLetter BL = P.B

fromPitchLetter :: P.PitchLetter -> PitchLetter
fromPitchLetter (P.C) = CL
fromPitchLetter (P.D) = DL
fromPitchLetter (P.E) = EL
fromPitchLetter (P.F) = FL
fromPitchLetter (P.G) = GL
fromPitchLetter (P.A) = AL
fromPitchLetter (P.B) = BL


toAlteration :: Accidental -> P.Alteration
toAlteration NO_ACCIDENTAL      = P.NAT
toAlteration DBL_FLAT           = P.DBL_FLAT
toAlteration FLAT               = P.FLAT
toAlteration NATURAL            = P.NAT
toAlteration SHARP              = P.SHARP
toAlteration DBL_SHARP          = P.DBL_SHARP

fromAlteration :: P.Alteration -> Accidental
fromAlteration P.DBL_FLAT       = DBL_FLAT
fromAlteration P.FLAT           = FLAT
fromAlteration P.NAT            = NO_ACCIDENTAL
fromAlteration P.SHARP          = SHARP
fromAlteration P.DBL_SHARP      = DBL_SHARP

toPitchRel :: Pitch -> P.Pitch -> P.Pitch
toPitchRel (Pitch l a om) p0@(P.Pitch lbl0 o0) = octaveAdjust root om
  where
    lbl1        = toNoteLabel l a
    dist_up     = P.arithmeticDistanceNL lbl0 lbl1
    dist_down   = P.arithmeticDistanceNL lbl1 lbl0
    root        = if dist_up > dist_down 
                    then firstAbove lbl1 p0 else firstBelow lbl1 p0


firstAbove :: P.NoteLabel -> P.Pitch -> P.Pitch
firstAbove lbl1 (P.Pitch lbl0@(P.NoteLabel ltr0 _) o0) = 
    P.Pitch lbl1 ove
  where
    dist = P.arithmeticDistanceNL lbl0 lbl1
    om   = if fromEnum ltr0 + dist > 7 then 1 else 0
    ove = o0 + om

firstBelow :: P.NoteLabel -> P.Pitch -> P.Pitch
firstBelow lbl1 (P.Pitch lbl0@(P.NoteLabel ltr0 _) o0) = 
    P.Pitch lbl1 ove
  where
    dist = P.arithmeticDistanceNL lbl0 lbl1
    om   = if fromEnum ltr0 - dist > 0 then (-1) else 0
    ove = o0 + om




octaveAdjust :: P.Pitch -> Octave -> P.Pitch
octaveAdjust pch@(P.Pitch lbl o) om = case om of
    OveDefault      -> pch
    OveRaised i     -> P.Pitch lbl (o+i)
    OveLowered i    -> P.Pitch lbl (o-i)
 


--------------------------------------------------------------------------------
-- Pretty printing helpers
