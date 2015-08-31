{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.BracketSyntax
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

module Payasan.Base.Internal.BracketSyntax
  ( 
    module Payasan.Base.Internal.CommonSyntax
  
  , Phrase(..)
  , Bar(..)
  , CtxElement(..)
  , Element(..)
  , Note(..)

  , sizeCtxElement

  ) where


import Payasan.Base.Internal.CommonSyntax

import Payasan.Base.Duration


import Data.Data



--------------------------------------------------------------------------------
-- Syntax

-- | Bracket syntax must be parametric on pitch so it can
-- handle nice LilyPond things like drums.
--
data Phrase pch = Phrase { phrase_bars :: [Bar pch] }
  deriving (Data,Eq,Show,Typeable)



-- | Note Beaming is not captured in parsing.
--
data Bar pch = Bar 
    { render_info       :: LocalRenderInfo
    , bar_elements      :: [CtxElement pch]
    }
  deriving (Data,Eq,Show,Typeable)

-- | Note Beaming is not captured in parsing.
--
data CtxElement pch = Atom    (Element pch)
                    | Beamed  [CtxElement pch]
                    | Tuplet  TupletSpec        [CtxElement pch]
  deriving (Data,Eq,Show,Typeable)


-- | Note is should be quite easy to add ties (as write-only)
-- to get long notes after beaming.
--
-- See old Neume code. 
--
data Element pch = NoteElem   (Note pch)
                 | Rest       Duration
                 | Chord      [pch]         Duration
                 | Graces     [Note pch]
  deriving (Data,Eq,Show,Typeable)


data Note pch = Note pch Duration
  deriving (Data,Eq,Show,Typeable)


sizeCtxElement :: CtxElement pch -> RDuration
sizeCtxElement (Atom e)            = sizeElement e
sizeCtxElement (Beamed es)         = sum $ map sizeCtxElement es
sizeCtxElement (Tuplet {})         = error "sizeCtxElement (Tuplet {})"

sizeElement :: Element pch -> RDuration
sizeElement (NoteElem (Note _ d))  = durationSize d
sizeElement (Rest d)               = durationSize d
sizeElement (Chord _ d)            = durationSize d
sizeElement (Graces {})            = 0
