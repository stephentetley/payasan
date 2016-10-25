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
-- Underscore suffix indicates a command.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.Utils
  ( 

  -- * Markup
    huge_
  , large_
  , normalSize_
  , small_
  , tiny_
  , teeny_

  -- * pretty printers 
  , ($?+$)

  , vsep
  , withString

  , command
  , value
  , quotedText
  , block 
  , anonBlock
  , simultaneous1
  , simultaneous
  , definition


  , score_
  , markup_
  , above
  , below
  , new_
  , newStaff_
  , newStaffDefn
  , voice_
  , newVoice_
  , newVoiceDefn
  , newVoiceWith_
  , newDrumVoice_
  , layout_
  , context_

  , newRhythmicStaff_
  , newDrumStaff_
  , newDrumStaffWith_

  , version_
  , title
  , relative_
  , absolute_
  , clef_
  , key_
  , mode_
  , time_
  , cadenzaOn_
  , cadenzaOff_
  , numericTimeSignature_
  , tuplet_
  , stemUp_
  , stemDown_
  , with_
  , withBlock_ 

  , consists_
  , override_
  , set_
  , hide_

  , drummode_

  , startGroup_
  , stopGroup_


  , tie
  , rest
  , spacer
  , skip

  , pitch
  , noteLength 
  , beamForm
  , chordForm
  , graceForm
  , tupletForm
 
  ) where

import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.SyntaxCommon

import qualified Payasan.Base.Pitch as P
import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJ hiding ( Mode, mode )       -- package: pretty


huge_           :: Doc -> Doc 
huge_ d         = command "huge" <+> d

large_          :: Doc -> Doc 
large_ d        = command "large" <+> d

normalSize_     :: Doc -> Doc 
normalSize_ d   = command "normalSize" <+> d

small_          :: Doc -> Doc 
small_ d        = command "small" <+> d


tiny_           :: Doc -> Doc 
tiny_ d         = command "tiny" <+> d

teeny_          :: Doc -> Doc 
teeny_ d        = command "teeny" <+> d


--------------------------------------------------------------------------------
-- Pretty printing helpers


vsep :: [Doc] -> Doc
vsep []     = empty
vsep [d]    = d
vsep (d:ds) = d $+$ vsep ds

withString :: String -> (String -> Doc) -> Doc
withString ss f = if null ss then empty else f ss




infixl 5 $?+$ 

($?+$) :: Maybe Doc -> Doc -> Doc
($?+$) (Nothing) d2 = d2
($?+$) (Just d1) d2 = d1 $+$ d2





command :: String -> Doc
command = text . ('\\' :)


value           :: String -> Doc
value ss        = text $ '#':ss

quotedText      :: String -> Doc
quotedText s    = doubleQuotes (text s)

block :: Maybe Doc -> Doc -> Doc
block prefix body = maybe inner (\d -> d <+> inner) prefix
  where
    inner = let d1 = nest 2 body in lbrace $+$ d1 $+$ rbrace

anonBlock :: Doc -> Doc
anonBlock doc  = block Nothing doc

simultaneous1 :: Doc -> Doc
simultaneous1 d = text "<<" $+$ d $+$ text ">>"

simultaneous :: [Doc] -> Doc
simultaneous xs = text "<<" $+$ step xs $+$ text ">>"
  where
    step [d]    = d   
    step (d:ds) = d $+$ text "//" $+$ step ds
    step []     = empty


definition :: String -> Doc -> Doc 
definition ss d = text ss <+> char '=' <+> d


score_          :: Doc -> Doc
score_          = block (Just $ command "score")
       
new_            :: Doc -> Doc
new_ d          = command "new" <+> d

markup_         :: Doc -> Doc
markup_ d       = command "markup" <+> d        


above           :: Doc -> Doc
above d         = char '^' <> d

below           :: Doc -> Doc
below d         = char '_' <> d



newStaff_       :: Doc
newStaff_       = new_ $ text "Staff"

newStaffDefn :: String -> Doc
newStaffDefn name = newStaff_ <+> char '=' <+> doubleQuotes (text name)


voice_          :: Doc
voice_          = command "Voice"

newVoice_       :: Doc
newVoice_       = new_ $ text "Voice"

newVoiceDefn :: String -> Doc
newVoiceDefn name = newVoice_ <+> char '=' <+> doubleQuotes (text name)

-- | supplied arg enclosed in a block
--
newVoiceWith_ :: Doc -> Doc
newVoiceWith_ d = newVoice_ <+> withBlock_ d

newDrumVoice_ :: Doc
newDrumVoice_ = new_ $ text "DrumVoice"


newRhythmicStaff_ :: Doc
newRhythmicStaff_ = command "new" <+> text "RhythmicStaff"

newDrumStaff_ :: Doc
newDrumStaff_ = command "new" <+> text "DrumStaff"

newDrumStaffWith_ :: Doc -> Doc
newDrumStaffWith_ d = newDrumStaff_ <+> withBlock_ d

layout_         :: Doc -> Doc
layout_         = block (Just $ command "layout")

context_        :: Doc -> Doc
context_        = block (Just $ command "context")


version_ :: String -> Doc
version_ ss = command "version" <+> doubleQuotes (text ss)

title :: String -> Doc
title ss = definition "title" (text ss)

relative_ :: P.Pitch -> Doc
relative_ pch = command "relative" <+> pitch (fromPitchAbs pch)

absolute_ :: Doc
absolute_ = command "absolute"


clef_ :: Clef -> Doc
clef_ TREBLE    = command "clef" <+> text "treble"
clef_ BASS      = command "clef" <+> text "bass"

key_ :: Key -> Doc
key_ (Key ps m)         = command "key" <+> pitchName ps <+> mode_ m


pitchName :: P.PitchName -> Doc
pitchName (P.PitchName l a) = 
    pitchLetter (fromPitchLetter l) <> fn (fromAlteration a)
  where
    fn NATURAL = empty
    fn x       = accidental x

mode_ :: Mode -> Doc
mode_ MAJOR             = command "major"
mode_ MINOR             = command "minor"
mode_ MIXOLYDIAN        = command "mixolydian"
mode_ DORIAN            = command "dorian"
mode_ PHRYGIAN          = command "phrygian"
mode_ LYDIAN            = command "lydian"
mode_ LOCRIAN           = command "locrian"


time_ :: Time -> Doc
time_ (Time n d) = command "time" <+> int n <> char '/' <> int d

cadenzaOn_      :: Doc
cadenzaOn_      = command "cadenzaOn"

cadenzaOff_     :: Doc
cadenzaOff_     = command "cadenzaOff"

numericTimeSignature_   :: Doc 
numericTimeSignature_   = command "numericTimeSignature"


tuplet_ :: TupletSpec -> Doc
tuplet_ (TupletSpec { tuplet_num = n, tuplet_time_mult = t}) = 
    command "tuplet" <+> int n <> char '/' <> int t


stemUp_         :: Doc
stemUp_         = command "stemUp"

stemDown_       :: Doc
stemDown_       = command "stemDown"


consists_       :: String -> Doc
consists_ ss    = command "consists" <+> doubleQuotes (text ss)

-- | Overrides are expected to be copy-paste fragments from 
-- LilyPond. 
--
-- So, it seems more efficient just to supply them with the 
-- quoted string.
-- 
override_       :: String -> Doc
override_ ss    = command "override" <+> text ss


-- | Uses of set_ are expected to be copy-paste fragments from 
-- LilyPond. 
--
set_            :: String -> Doc
set_ ss         = command "set" <+> text ss

-- | Uses of hide are expected to be copy-paste fragments from 
-- LilyPond. 
--
hide_           :: String -> Doc
hide_ ss        = command "hide" <+> text ss



with_           :: Doc
with_           = command "with"

withBlock_      :: Doc -> Doc
withBlock_ d    = with_ <+> anonBlock d


drummode_       :: Doc
drummode_       = command "drummode"

startGroup_     :: Doc
startGroup_     = command "startGroup"

stopGroup_      :: Doc
stopGroup_      = command "stopGroup"





tie :: Tie -> Doc
tie NO_TIE = empty
tie TIE    = char '~'



rest :: LyNoteLength -> Doc
rest d = char 'r' <> noteLength d

spacer :: LyNoteLength -> Doc
spacer d = char 's' <> noteLength d

skip :: LyNoteLength -> Doc
skip d = command "skip" <> noteLength d


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
chordForm ds = char '<' <> hsep ds <> char '>'

graceForm :: [Doc] -> Doc
graceForm ds = command "grace" <+> braces (hsep ds)


beamForm :: [Doc] -> Doc
beamForm (d:ds) = d <> char '[' <> hsep ds <> char ']'
beamForm []     = empty


tupletForm :: TupletSpec -> [Doc] -> Doc
tupletForm spec notes = tuplet_ spec <+> braces (hsep notes) 