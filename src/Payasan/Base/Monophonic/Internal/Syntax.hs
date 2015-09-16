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
    module Payasan.Base.Internal.CommonSyntax

  , GenMonoPhrase
  , GenMonoBar
  , GenMonoNoteGroup
  , GenMonoElement

  , GenMonoLyPhrase
  , GenMonoLyBar
  , GenMonoLyNoteGroup
  , GenMonoLyElement
  
  , StdMonoPhrase
  , ABCMonoPhrase
  , LyMonoPhrase


  , Phrase(..)

  , Bar(..)
  , NoteGroup(..)
  , Element(..)

  , pushLocalRenderInfo
  , sizeNoteGroup

  ) where

import qualified Payasan.Base.Internal.ABC.Syntax as ABC
import Payasan.Base.Internal.CommonSyntax
import qualified Payasan.Base.Internal.LilyPond.Syntax as LY
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Data.Data

--------------------------------------------------------------------------------
-- Syntax



type GenMonoPhrase pch          = Phrase pch Duration
type GenMonoBar pch             = Bar pch Duration
type GenMonoNoteGroup pch       = NoteGroup pch Duration
type GenMonoElement pch         = Element pch Duration

type GenMonoLyPhrase pch        = Phrase pch LY.NoteLength
type GenMonoLyBar pch           = Bar pch LY.NoteLength
type GenMonoLyNoteGroup pch     = NoteGroup pch LY.NoteLength
type GenMonoLyElement pch       = Element pch LY.NoteLength

-- | Parametric on pitch so we can have the same syntax to 
-- represent scale degrees, drum notes, etc.
--
-- Parametric on duration so we can read ABC and decode duration
-- multipliers in a post-parsing phase.
--
data Phrase pch drn = Phrase { phrase_bars :: [Bar pch drn] }
  deriving (Data,Eq,Show,Typeable)

type StdMonoPhrase          = Phrase Pitch Duration

type ABCMonoPhrase          = Phrase ABC.Pitch ABC.NoteLength
type LyMonoPhrase           = Phrase LY.Pitch  LY.NoteLength



-- | Note Beaming is not captured in parsing.
--
data Bar pch drn = Bar 
    { bar_header        :: LocalRenderInfo
    , bar_elements      :: [NoteGroup pch drn]
    }
  deriving (Data,Eq,Show,Typeable)




-- | Note Beaming is added in rendering.
--
-- Tuplets seem essential 
--
data NoteGroup pch drn = Atom    (Element pch drn)
                       | Tuplet  TupletSpec         [NoteGroup pch drn]
  deriving (Data,Eq,Show,Typeable)


-- | TODO - if we allow ties, there is a simple duration
-- doubling transformation (unfortunately there isn\'t a
-- simple duration halving trafo).
--
--
data Element pch drn = Note   pch   drn
                     | Rest   drn
  deriving (Data,Eq,Show,Typeable)



--------------------------------------------------------------------------------
-- Push RenderInfo into bars.


pushLocalRenderInfo :: LocalRenderInfo -> Phrase pch drn -> Phrase pch drn
pushLocalRenderInfo ri (Phrase bs) = Phrase $ map upd bs
  where
    upd bar = bar { bar_header = ri }



sizeNoteGroup :: NoteGroup pch Duration -> RDuration
sizeNoteGroup (Atom e)          = sizeElement e
sizeNoteGroup (Tuplet {})       = error "sizeNoteGroup (Tuplet {})"

sizeElement :: Element pch Duration -> RDuration
sizeElement (Note _ d)          = durationSize d
sizeElement (Rest d)            = durationSize d
