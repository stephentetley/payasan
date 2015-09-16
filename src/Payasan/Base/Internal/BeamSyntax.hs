{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.BeamSyntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist segmented into bars, with notes, rests, 
-- chords, grace notes and triplets.
--
-- Intermediate syntax for beam grouping.
--
-- Parametric on pitch for LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.BeamSyntax
  ( 

    Phrase(..)
  , Bar(..)
  , NoteGroup(..)
  , Element(..)
  , Note(..)

  -- * Operations
  , pushLocalRenderInfo
  , sizeNoteGroup
  , firstRenderInfo 


  ) where


import Payasan.Base.Internal.CommonSyntax

import Payasan.Base.Duration


import Data.Data



--------------------------------------------------------------------------------
-- Syntax

-- | Bracket syntax must be parametric on pitch so it can
-- handle nice LilyPond things like drums.
--
data Phrase pch drn = Phrase { phrase_bars :: [Bar pch drn] }
  deriving (Data,Eq,Show,Typeable)



-- | Note Beaming is not captured in parsing.
--
data Bar pch drn = Bar 
    { bar_header        :: LocalRenderInfo
    , bar_elements      :: [NoteGroup pch drn]
    }
  deriving (Data,Eq,Show,Typeable)

-- | Note - Beaming is not captured in parsing, but it is 
-- synthesized again for output.
--
-- Beams must allow nesting 
--
data NoteGroup pch drn = Atom     (Element pch drn)
                       | Beamed   [NoteGroup pch drn]
                       | Tuplet   TupletSpec            [NoteGroup pch drn]
  deriving (Data,Eq,Show,Typeable)


-- | Note is should be quite easy to add ties (as write-only)
-- to get long notes after beaming.
--
-- See old Neume code. 
--
data Element pch drn = NoteElem   (Note pch drn)
                     | Rest       drn
                     | Chord      [pch]           drn
                     | Graces     [Note pch drn]
  deriving (Data,Eq,Show,Typeable)


data Note pch drn = Note pch drn
  deriving (Data,Eq,Show,Typeable)

--------------------------------------------------------------------------------
-- Operations (maybe should be in another module)

pushLocalRenderInfo :: LocalRenderInfo -> Phrase pch drn -> Phrase pch drn
pushLocalRenderInfo ri (Phrase bs) = Phrase $ map upd bs
  where
    upd bar = bar { bar_header = ri }


sizeNoteGroup :: NoteGroup pch Duration -> RDuration
sizeNoteGroup (Atom e)            = sizeElement e
sizeNoteGroup (Beamed es)         = sum $ map sizeNoteGroup es
sizeNoteGroup (Tuplet {})         = error "sizeNoteGroup (Tuplet {})"

sizeElement :: Element pch Duration -> RDuration
sizeElement (NoteElem (Note _ d))  = durationSize d
sizeElement (Rest d)               = durationSize d
sizeElement (Chord _ d)            = durationSize d
sizeElement (Graces {})            = 0


firstRenderInfo :: Phrase pch drn -> Maybe LocalRenderInfo
firstRenderInfo (Phrase [])    = Nothing
firstRenderInfo (Phrase (b:_)) = Just $ bar_header b
