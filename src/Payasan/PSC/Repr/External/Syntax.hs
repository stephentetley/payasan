{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.External.Syntax
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist segmented into bars, with notes, rests, 
-- chords, grace notes and triplets.
--
-- Parametric on Duration and Pitch.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.External.Syntax
  ( 
   
    StdPart
  , StdSection
  , StdBar
  , StdNoteGroup
  , StdElement
  , StdGrace1

  , StdPart1
  , StdSection1
  , StdBar1
  , StdNoteGroup1
  , StdElement1

  , Part(..)
  , Section(..)
  , Bar(..)
  , NoteGroup(..)
  , Element(..)
  , Grace1(..)

  -- * Concrete syntax fragments
  , GenLySectionQuote(..)
  , LySectionQuote(..)
  , ABCSectionQuote(..)

  , specializeGenLySectionQuote
  
  , GenLyPartOut
  , LyPartOut
  , ABCPartOut
  
  -- * Operations
  , pushSectionInfo
  , sizeNoteGroup
  , initialSectionInfo

  , extractSectionInfos

  ) where


import Payasan.PSC.Base.ABCCommon
import Payasan.PSC.Base.LilyPondCommon
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch

import Data.Data



--------------------------------------------------------------------------------
-- Syntax


type StdPart            = Part      Pitch Duration () 
type StdSection         = Section   Pitch Duration ()
type StdBar             = Bar       Pitch Duration () 
type StdNoteGroup       = NoteGroup Pitch Duration () 
type StdElement         = Element   Pitch Duration ()
type StdGrace1          = Grace1    Pitch Duration

type StdPart1 anno      = Part      Pitch Duration anno
type StdSection1 anno   = Section   Pitch Duration anno
type StdBar1 anno       = Bar       Pitch Duration anno
type StdNoteGroup1 anno = NoteGroup Pitch Duration anno
type StdElement1 anno   = Element   Pitch Duration anno




data Part pch drn anno = Part { part_sections :: [Section pch drn anno] }
  deriving (Data,Eq,Show,Typeable)
  
-- Note - the musical structures in Payasan don't "feel" monoidal.
-- Specifically, appending a smaller element to a larger container
-- feels more natural than monoidal concatenation.
-- 
-- E.g. it seems natural to add (append) a Section to a Part, but 
-- while we can concat Parts (the Monoid instance makes sense)
-- doing so doesn't seem a natral way to build music - a Part 
-- feels singular, self contained.
--

instance Monoid (Part pch drn anno) where
  mempty = Part []
  Part xs `mappend` Part ys = Part $ xs ++ ys



-- | Including section name gives an easily addressable scheme
-- for extra render information.
-- 
-- E.g we could support MIDI rendering with a list (dictionary) 
-- of BPM (tempo) changes for each named section. This removes
-- the need to store BPM in the AST (extensibility is serverely
-- hampered if we have to store all information in the AST).
--   
-- It would be up to the user whether names are unique - there 
-- is value in not being strict about this (musically sections
-- often have a fairly loose names as in section schemes like 
-- "AAB").
--
data Section pch drn anno = Section
    { section_name      :: String
    , section_info      :: !SectionInfo
    , section_bars      :: [Bar pch drn anno]
    }
  deriving (Data,Eq,Show,Typeable)

-- | Change - Beaming is now captured in parsing.
--   Change - SectionInfo promoted to (new element) Section.
--
data Bar pch drn anno = Bar { bar_groups :: [NoteGroup pch drn anno] }
  deriving (Data,Eq,Show,Typeable)

-- | Note Beaming is not captured in parsing.
--
data NoteGroup pch drn anno = 
      Atom     (Element pch drn anno)
    | Beamed   [NoteGroup pch drn anno]
    | Tuplet   TupletSpec        [NoteGroup pch drn anno]
  deriving (Data,Eq,Show,Typeable)


-- | Note is should be quite easy to add ties (as write-only)
-- to get long notes after beaming.
--
-- See old Neume code. 
--
-- Punctuation is for LilyPond only (may change).
-- 
-- Skip is essentially a rest but they have different 
-- interpretations in LilyPond and need to be 
-- differentiated.
--
data Element pch drn anno = 
      Note          pch     drn anno   Tie
    | Rest          drn
    | Spacer        drn
    | Skip          drn
    | Chord         [pch]   drn anno   Tie
    | Graces        [Grace1 pch drn]
    | Punctuation   String
  deriving (Data,Eq,Show,Typeable)


data Grace1 pch drn = Grace1 pch drn
  deriving (Data,Eq,Show,Typeable)

--------------------------------------------------------------------------------
-- Quasiquote / TH fragments

-- Notes
-- LySectionQuote & GenLySectionQuote represent what is parsed
--
-- There is no meter / key info in parsed fragments so we omit 
-- them. We do not attempt to synthesize them with defaults.

newtype GenLySectionQuote pch anno = 
    GenLySectionQuote { getGenLySectionQuote :: [Bar pch LyNoteLength anno] } 
    deriving (Data,Eq,Show,Typeable)

newtype LySectionQuote anno = 
    LySectionQuote { getLySectionQuote :: [Bar LyPitch LyNoteLength anno] } 
    deriving (Data,Eq,Show,Typeable)
      
newtype ABCSectionQuote = 
    ABCSectionQuote { getABCSection :: [Bar ABCPitch ABCNoteLength ()] } 
    deriving (Data,Eq,Show,Typeable)

    
specializeGenLySectionQuote :: GenLySectionQuote LyPitch anno 
                            -> LySectionQuote anno
specializeGenLySectionQuote = LySectionQuote . getGenLySectionQuote


-- These defs need not go in this module. 
--
-- For extensibility (clients should be able to define their own 
-- backends) specific backend code could be outside the 
-- Payasan.PSC.Repr.External namespace. However LilyPond and ABC 
-- are so fundamental that it is harmless to define them here.
--


type GenLyPartOut pch anno      = Part pch LyNoteLength anno

type LyPartOut anno             = Part LyPitch LyNoteLength anno

type ABCPartOut anno            = Part ABCPitch ABCNoteLength anno


--------------------------------------------------------------------------------
-- Operations

pushSectionInfo :: SectionInfo 
                -> Part pch drn anno 
                -> Part pch drn anno
pushSectionInfo _ (Part {}) = 
    error $ "pushSectionInfo - should now be redundant"


sizeNoteGroup :: NoteGroup pch Duration anno -> RatDuration
sizeNoteGroup (Atom e)              = sizeElement e
sizeNoteGroup (Beamed es)           = sum $ map sizeNoteGroup es
sizeNoteGroup (Tuplet spec es)      = tupletUnitRatDuration spec (firstOf es)
  where
    firstOf (x:_)   = sizeNoteGroup x
    firstOf []      = durationToRatDuration d_eighth

sizeElement :: Element pch Duration anno -> RatDuration
sizeElement (Note _ d _ _)              = durationToRatDuration d
sizeElement (Rest d)                    = durationToRatDuration d
sizeElement (Spacer d)                  = durationToRatDuration d
sizeElement (Skip d)                    = durationToRatDuration d
sizeElement (Chord _ d _ _)             = durationToRatDuration d
sizeElement (Graces {})                 = 0
sizeElement (Punctuation {})            = 0


-- | Returns default section info (4/4 time, C major) if the 
-- Part has no Sections.
-- 
-- Pragmatically this is okay, SectionInfo has reasonable
-- defaults and we want to avoid Maybe proliferations in e.g 
-- rewrite traversals.
--
initialSectionInfo :: Part pch drn anno -> SectionInfo
initialSectionInfo (Part [])    = default_section_info
initialSectionInfo (Part (s:_)) = section_info s


extractSectionInfos :: Part pch drn anno -> [SectionInfo]
extractSectionInfos = map section_info . part_sections




