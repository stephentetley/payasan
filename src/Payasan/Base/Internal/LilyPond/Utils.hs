{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.Utils
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

module Payasan.Base.Internal.LilyPond.Utils
  ( 


  -- * Conversion
    toPitchAbs
  , fromPitchAbs

  , toPitchSpelling
  , fromPitchSpelling

  , toPitchLetter
  , fromPitchLetter
  , toAlteration
  , fromAlteration

  , toPitchRel
  , fromPitchRel

  -- * Output
  , command
  , block 
  , definition

  , version
  , title
  , relative
  , absolute
  , key
  , mode
  , meter
  , tupletSpec

  , rest

  , pitch
  , noteLength 
  , beamForm
  , chordForm
  , graceForm
 
  ) where

import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.CommonSyntax

import qualified Payasan.Base.Pitch as P
import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJ hiding ( Mode, mode )       -- package: pretty

--------------------------------------------------------------------------------
-- Conversion


-- | CU = middle C (octave 4)
--
toPitchAbs :: Pitch -> P.Pitch
toPitchAbs (Pitch l a om) =
    let lbl = P.PitchSpelling (toPitchLetter l) (toAlteration a) 
    in P.Pitch lbl (toOctaveAbs om)



fromPitchAbs :: P.Pitch -> Pitch
fromPitchAbs (P.Pitch lbl o) = 
    let (l,a) = fromPitchSpelling lbl
    in Pitch l a (fromOctaveAbs o)



toPitchSpelling :: PitchLetter -> Accidental -> P.PitchSpelling
toPitchSpelling l a = P.PitchSpelling (toPitchLetter l) (toAlteration a)

fromPitchSpelling :: P.PitchSpelling -> (PitchLetter,Accidental)
fromPitchSpelling (P.PitchSpelling p a) = (fromPitchLetter p, fromAlteration a)


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


makeOctaveModifier :: Int -> Octave
makeOctaveModifier i 
    | i == 0    = OveDefault
    | i >  0    = OveRaised i
    | otherwise = OveLowered (abs i)


fromPitchRel :: P.Pitch -> P.Pitch -> Pitch
fromPitchRel p1 p_old = 
    let om    = makeOctaveModifier $ P.lyOctaveDistance p_old p1
        (l,a) = fromPitchSpelling $ P.pitch_spelling p1        
    in Pitch l a om


toPitchRel :: Pitch -> P.Pitch -> P.Pitch
toPitchRel (Pitch l a om) p0 = octaveAdjust root om
  where
    lbl   = toPitchSpelling l a
    p1    = P.Pitch lbl (P.pitch_octave p0)
    root  = if P.arithmeticDistance p0 p1 <= 5 
              then p1 else P.Pitch lbl (P.pitch_octave p0 - 1)



octaveAdjust :: P.Pitch -> Octave -> P.Pitch
octaveAdjust pch@(P.Pitch lbl o) om = case om of
    OveDefault      -> pch
    OveRaised i     -> P.Pitch lbl (o+i)
    OveLowered i    -> P.Pitch lbl (o-i)


                   

--------------------------------------------------------------------------------
-- Pretty printing helpers


command :: String -> Doc
command = text . ('\\' :)

block :: Maybe Doc -> Doc -> Doc
block prefix body = maybe inner (\d -> d <+> inner) prefix
  where
    inner = let d1 = nest 2 body in lbrace $+$ d1 $+$ rbrace

definition :: String -> Doc -> Doc 
definition ss d = text ss <+> char '=' <+> doubleQuotes d


version :: String -> Doc
version ss = command "version" <+> doubleQuotes (text ss)

title :: String -> Doc
title ss = definition "title" (text ss)

relative :: P.Pitch -> Doc
relative pch = command "relative" <+> pitch (fromPitchAbs pch)

absolute :: Doc
absolute = command "absolute"


key :: Key -> Doc
key (Key ps m)          = command "key" <+> pitchSpelling ps <+> mode m

pitchSpelling :: P.PitchSpelling -> Doc
pitchSpelling (P.PitchSpelling l a) = 
    pitchLetter (fromPitchLetter l) <> fn (fromAlteration a)
  where
    fn NATURAL = empty
    fn x       = accidental x

mode :: Mode -> Doc
mode MAJOR              = command "major"
mode MINOR              = command "minor"
mode MIXOLYDIAN         = command "mixolydian"
mode DORIAN             = command "dorian"
mode PHRYGIAN           = command "phrygian"
mode LYDIAN             = command "lydian"
mode LOCRIAN            = command "locrian"


meter :: Meter -> Doc
meter (Meter n d) = command "time" <+> int n <> char '/' <> int d

tupletSpec :: TupletSpec -> Doc
tupletSpec (TupletSpec { tuplet_num = n, tuplet_time = t}) = 
    command "tuplet" <+> int n <> char '/' <> int t
     


rest :: NoteLength -> Doc
rest d = char 'r' <> noteLength d


pitch :: Pitch -> Doc
pitch (Pitch l a om)  = pitchLetter l <> accidental a <> octaveModifier om

pitchLetter :: PitchLetter -> Doc
pitchLetter CL  = char 'c'
pitchLetter DL  = char 'd'
pitchLetter EL  = char 'e'
pitchLetter FL  = char 'f'
pitchLetter GL  = char 'g'
pitchLetter AL  = char 'a'
pitchLetter BL  = char 'b'

accidental :: Accidental -> Doc
accidental NO_ACCIDENTAL        = empty
accidental DBL_FLAT             = text "eses"
accidental FLAT                 = text "es"
accidental NATURAL              = text "!"
accidental SHARP                = text "is"
accidental DBL_SHARP            = text "isis"


octaveModifier :: Octave -> Doc
octaveModifier (OveDefault)   = empty
octaveModifier (OveRaised i)  = text $ replicate i '\''
octaveModifier (OveLowered i) = text $ replicate i ','


noteLength :: NoteLength -> Doc
noteLength (DrnDefault)     = empty
noteLength (DrnExplicit d)  = duration d

duration :: Duration -> Doc
duration d = let (root,dc) = lilyPondComponents d
                 dotd      = text $ replicate dc '.'
             in either command int root <> dotd


chordForm :: [Doc] -> Doc
chordForm ds = char '<' <> hcat ds <> char '>'

graceForm :: [Doc] -> Doc
graceForm ds = command "grace" <+> braces (hsep ds)


beamForm :: [Doc] -> Doc
beamForm (d:ds) = d <> char '[' <> hsep ds <> char ']'
beamForm []     = empty