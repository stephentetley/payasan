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



type GenMonoPhrase pch anno         = Phrase    pch Duration anno
type GenMonoBar pch anno            = Bar       pch Duration anno
type GenMonoNoteGroup pch anno      = NoteGroup pch Duration anno
type GenMonoElement pch anno        = Element   pch Duration anno

type GenLyMonoPhrase pch anno       = Phrase    pch LY.NoteLength anno
type GenLyMonoBar pch anno          = Bar       pch LY.NoteLength anno
type GenLyMonoNoteGroup pch anno    = NoteGroup pch LY.NoteLength anno
type GenLyMonoElement pch anno      = Element   pch LY.NoteLength anno

type ABCMonoPhrase                  = Phrase    ABC.Pitch ABC.NoteLength ()
type ABCMonoBar                     = Bar       ABC.Pitch ABC.NoteLength ()
type ABCMonoNoteGroup               = NoteGroup ABC.Pitch ABC.NoteLength ()
type ABCMonoElement                 = Element   ABC.Pitch ABC.NoteLength ()

type StdMonoPhrase                  = Phrase Pitch Duration ()

type LyMonoPhrase anno              = Phrase LY.Pitch  LY.NoteLength anno


-- | Parametric on pitch so we can have the same syntax to 
-- represent scale degrees, drum notes, etc.
--
-- Parametric on duration so we can read ABC and decode duration
-- multipliers in a post-parsing phase.
--
data Phrase pch drn anno = Phrase { phrase_bars :: [Bar pch drn anno] }
  deriving (Data,Eq,Show,Typeable)




-- | Note Beaming is not captured in parsing.
--
data Bar pch drn anno = Bar 
    { bar_header        :: LocalRenderInfo
    , bar_elements      :: [NoteGroup pch drn anno]
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


-- | TODO - if we allow ties, there is a simple duration
-- doubling transformation (unfortunately there isn\'t a
-- simple duration halving trafo).
--
--
data Element pch drn anno = 
      Note   pch   drn   anno
    | Rest   drn
  deriving (Data,Eq,Show,Typeable)



--------------------------------------------------------------------------------
-- Push RenderInfo into bars.


pushLocalRenderInfo :: LocalRenderInfo 
                    -> Phrase pch drn anno 
                    -> Phrase pch drn anno
pushLocalRenderInfo ri (Phrase bs) = Phrase $ map upd bs
  where
    upd bar = bar { bar_header = ri }



sizeNoteGroup :: NoteGroup pch Duration anno -> RDuration
sizeNoteGroup (Atom e)          = sizeElement e
sizeNoteGroup (Tuplet {})       = error "sizeNoteGroup (Tuplet {})"

sizeElement :: Element pch Duration anno -> RDuration
sizeElement (Note _ d _)        = durationSize d
sizeElement (Rest d)            = durationSize d
