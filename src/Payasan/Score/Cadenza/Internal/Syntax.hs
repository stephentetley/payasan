{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.Syntax
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist without bars, but with beam groups.
--
--------------------------------------------------------------------------------

module Payasan.Score.Cadenza.Internal.Syntax
  ( 

    LySectionQuote(..)

  , Section(..)
  , NoteGroup(..)
  , Element(..)


  , pushSectionInfo
  , sectionInfo
  , sizeNoteGroup

  ) where

import Payasan.PSC.LilyPond.Base hiding  ( LySectionQuote(..) )

import Payasan.PSC.Base.SyntaxCommon
import Payasan.Base.Duration

import Data.Data

--------------------------------------------------------------------------------
-- Syntax

-- NOTE - we could accommodate ABC but parsing the input syntax
-- is thorny (whitespace sensitive) and ABC is already a second 
-- class citizen in Payasan because it has no rival to all the
-- nice modes in LilyPond (percussion etc.).


newtype LySectionQuote pch anno = 
    LySectionQuote { getLySectionQuote :: [NoteGroup pch LyNoteLength anno] } 
    deriving (Data,Eq,Show,Typeable)


-- | Parametric on pitch so we can have the same syntax to 
-- represent scale degrees, drum notes, etc.
--
-- Parametric on duration so we can read LilyPond and decode
-- omitted durations in a post-parsing phase.
--
-- LocalRenderInfo is annotated at the Section level - while this
-- prevents concatenation it simplifies transformation.
-- 
data Section pch drn anno = Section
    { section_name      :: String
    , section_info      :: !SectionInfo
    , section_groups    :: [NoteGroup pch drn anno] 
    }
  deriving (Data,Eq,Show,Typeable)





-- | Beaming should be hand coded.
--
-- Tuplets are simplified - no nesting.
--
data NoteGroup pch drn anno = 
      Atom    (Element pch drn anno)
    | Beamed  [NoteGroup pch drn anno]
    | Tuplet  TupletSpec         [Element pch drn anno]
  deriving (Data,Eq,Show,Typeable)


-- Element currently follows Elementary syntax (omitting chords 
-- and graces).
--
data Element pch drn anno = 
      Note          pch drn   anno  Tie
    | Rest          drn
    | Spacer        drn
    | Skip          drn
    | Punctuation   String
  deriving (Data,Eq,Show,Typeable)



--------------------------------------------------------------------------------
-- Push SectionInfo into a phrase.


pushSectionInfo :: SectionInfo 
                -> Section pch drn anno 
                -> Section pch drn anno
pushSectionInfo info s = s { section_info = info }


sectionInfo :: Section pch drn anno -> SectionInfo
sectionInfo = section_info




sizeNoteGroup :: NoteGroup pch Duration anno -> RatDuration
sizeNoteGroup (Atom e)          = sizeElement e
sizeNoteGroup (Beamed gs)       = sum $ map sizeNoteGroup gs
sizeNoteGroup (Tuplet spec es)  = tupletUnitRatDuration spec (firstOf es)
  where
    firstOf (x:_)   = sizeElement x
    firstOf []      = durationToRatDuration d_eighth

sizeElement :: Element pch Duration anno -> RatDuration
sizeElement (Note _ d _ _)          = durationToRatDuration d
sizeElement (Rest d)                = durationToRatDuration d
sizeElement (Spacer d)              = durationToRatDuration d
sizeElement (Skip d)                = durationToRatDuration d
sizeElement (Punctuation {})        = 0


