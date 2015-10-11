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
-- Helpers for LilyPond output (pretty printers).
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.Utils
  ( 

  -- * Output
    Markup
 
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

                   
newtype Markup = Markup { getMarkup :: Doc }

instance Monoid Markup where
  mempty = Markup $ empty
  a `mappend` b = Markup $ getMarkup a <> getMarkup b






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