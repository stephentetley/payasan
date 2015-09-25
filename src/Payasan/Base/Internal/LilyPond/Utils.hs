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

  , toPitchName
  , fromPitchName

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
toPitchAbs :: LyPitch -> P.Pitch
toPitchAbs (LyPitch l a om) =
    let lbl = P.PitchName (toPitchLetter l) (toAlteration a) 
    in P.Pitch lbl (toOctaveAbs om)



fromPitchAbs :: P.Pitch -> LyPitch
fromPitchAbs (P.Pitch lbl o) = 
    let (l,a) = fromPitchName lbl
    in LyPitch l a (fromOctaveAbs o)



toPitchName :: PitchLetter -> Accidental -> P.PitchName
toPitchName l a = P.PitchName (toPitchLetter l) (toAlteration a)

fromPitchName :: P.PitchName -> (PitchLetter,Accidental)
fromPitchName (P.PitchName p a) = (fromPitchLetter p, fromAlteration a)


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
toPitchLetter C = P.C
toPitchLetter D = P.D
toPitchLetter E = P.E
toPitchLetter F = P.F
toPitchLetter G = P.G
toPitchLetter A = P.A
toPitchLetter B = P.B

fromPitchLetter :: P.PitchLetter -> PitchLetter
fromPitchLetter (P.C) = C
fromPitchLetter (P.D) = D
fromPitchLetter (P.E) = E
fromPitchLetter (P.F) = F
fromPitchLetter (P.G) = G
fromPitchLetter (P.A) = A
fromPitchLetter (P.B) = B


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


fromPitchRel :: P.Pitch -> P.Pitch -> LyPitch
fromPitchRel p_old p1 = 
    let om    = makeOctaveModifier $ P.lyOctaveDistance p_old p1
        (l,a) = fromPitchName $ P.pitch_name p1        
    in LyPitch l a om




-- | In relative mode, the octave modifier is oblivious
-- to the initial calculation of the pitch /within a fourth/.
--
toPitchRel :: P.Pitch -> LyPitch -> P.Pitch
toPitchRel p0 (LyPitch l a om) = octaveAdjust root om
  where
    lbl   = toPitchName l a
    psame = P.Pitch lbl (P.pitch_octave p0)
    pup   = octaveAdjust psame $ OveRaised 1
    pdown = octaveAdjust psame $ OveLowered 1
    root  = withinFourth p0 psame pup pdown

withinFourth :: P.Pitch -> P.Pitch -> P.Pitch -> P.Pitch -> P.Pitch
withinFourth p0 a b c
    | P.arithmeticDistance p0 a <= 4    = a
    | P.arithmeticDistance p0 b <= 4    = b
    | otherwise                         = c



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
key (Key ps m)          = command "key" <+> pitchName ps <+> mode m

pitchName :: P.PitchName -> Doc
pitchName (P.PitchName l a) = 
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
tupletSpec (TupletSpec { tuplet_num = n, tuplet_time_mult = t}) = 
    command "tuplet" <+> int n <> char '/' <> int t
     


rest :: LyNoteLength -> Doc
rest d = char 'r' <> noteLength d


pitch :: LyPitch -> Doc
pitch (LyPitch l a om)  = pitchLetter l <> accidental a <> octaveModifier om

pitchLetter :: PitchLetter -> Doc
pitchLetter C   = char 'c'
pitchLetter D   = char 'd'
pitchLetter E   = char 'e'
pitchLetter F   = char 'f'
pitchLetter G   = char 'g'
pitchLetter A   = char 'a'
pitchLetter B   = char 'b'

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


noteLength :: LyNoteLength -> Doc
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