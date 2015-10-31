{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monomorphic.Internal.Syntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Very simple symbolic notelist with notes, rests, 
-- and triplets (no chords or graces).
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.Syntax
  ( 

    GenMonoPhrase
  , GenMonoBar
  , GenMonoNoteGroup
  , GenMonoElement

  , GenLyMonoPhrase
  , GenLyMonoBar
  , GenLyMonoNoteGroup
  , GenLyMonoElement

  , ABCMonoPhrase
  , ABCMonoBar
  , ABCMonoNoteGroup
  , ABCMonoElement
  
  , StdMonoPhrase
  , StdMonoBar
  , StdMonoNoteGroup
  , StdMonoElement

  , LyMonoPhrase


  , Phrase(..)
  , Bar(..)
  , NoteGroup(..)
  , Element(..)

  , pushContextInfo
  , contextInfo
  , sizeNoteGroup

  ) where

import Payasan.Base.Internal.ABC.Syntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Data.Data

--------------------------------------------------------------------------------
-- Syntax



type GenMonoPhrase pch anno         = Phrase    pch Duration anno
type GenMonoBar pch anno            = Bar       pch Duration anno
type GenMonoNoteGroup pch anno      = NoteGroup pch Duration anno
type GenMonoElement pch anno        = Element   pch Duration anno

type GenLyMonoPhrase pch anno       = Phrase    pch LyNoteLength anno
type GenLyMonoBar pch anno          = Bar       pch LyNoteLength anno
type GenLyMonoNoteGroup pch anno    = NoteGroup pch LyNoteLength anno
type GenLyMonoElement pch anno      = Element   pch LyNoteLength anno

type ABCMonoPhrase                  = Phrase    ABCPitch ABCNoteLength ()
type ABCMonoBar                     = Bar       ABCPitch ABCNoteLength ()
type ABCMonoNoteGroup               = NoteGroup ABCPitch ABCNoteLength ()
type ABCMonoElement                 = Element   ABCPitch ABCNoteLength ()

type StdMonoPhrase                  = Phrase    Pitch Duration ()
type StdMonoBar                     = Bar       Pitch Duration ()
type StdMonoNoteGroup               = NoteGroup Pitch Duration ()
type StdMonoElement                 = Element   Pitch Duration ()


type LyMonoPhrase anno              = Phrase LyPitch  LyNoteLength anno


-- | Parametric on pitch so we can have the same syntax to 
-- represent scale degrees, drum notes, etc.
--
-- Parametric on duration so we can read ABC and decode duration
-- multipliers in a post-parsing phase.
--
-- LocalRenderInfo is annotated at the Phrase level - while this
-- prevents concatenation it simplifies transformation.
-- 
data Phrase pch drn anno = Phrase 
    { phrase_header     :: !LocalContextInfo
    , phrase_bars       :: [Bar pch drn anno] 
    }
  deriving (Data,Eq,Show,Typeable)



-- | Note Beaming is not captured in parsing.
--
data Bar pch drn anno = Bar 
    { bar_elements      :: [NoteGroup pch drn anno]
    }
  deriving (Data,Eq,Show,Typeable)




-- | Note Beaming is added in rendering.
--
-- Tuplets seem essential 
--
data NoteGroup pch drn anno = 
      Atom    (Element pch drn anno)
    | Tuplet  TupletSpec         [NoteGroup pch drn anno]
  deriving (Data,Eq,Show,Typeable)


-- | TODO - should we implement ties?
--
-- Note - if we ignore anno Element can also represent 
-- Maybe+duration.
--
data Element pch drn anno = 
      Note          pch   drn   anno  Tie   Markup
    | Rest          drn
    | Spacer          drn
    | Skip          drn
    | Punctuation   String
  deriving (Data,Eq,Show,Typeable)



--------------------------------------------------------------------------------
-- Push RenderInfo into bars.


pushContextInfo :: LocalContextInfo 
                -> Phrase pch drn anno 
                -> Phrase pch drn anno
pushContextInfo ri (Phrase { phrase_bars = bs }) = 
    Phrase { phrase_header = ri
           , phrase_bars   = bs }


contextInfo :: Phrase pch drn anno -> LocalContextInfo
contextInfo = phrase_header


sizeNoteGroup :: NoteGroup pch Duration anno -> RDuration
sizeNoteGroup (Atom e)          = sizeElement e
sizeNoteGroup (Tuplet spec es)  = tupletUnitRDuration spec (firstOf es)
  where
    firstOf (x:_)   = sizeNoteGroup x
    firstOf []      = durationSize d_eighth

sizeElement :: Element pch Duration anno -> RDuration
sizeElement (Note _ d _ _ _)    = durationSize d
sizeElement (Rest d)            = durationSize d
sizeElement (Spacer d)          = durationSize d
sizeElement (Skip d)            = durationSize d
sizeElement (Punctuation {})    = 0


