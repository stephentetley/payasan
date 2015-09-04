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
    module Payasan.Base.Internal.CommonSyntax
  
  , Phrase(..)
  , Bar(..)
  , CtxElement(..)
  , Element(..)
  , Note(..)

  , pushLocalRenderInfo
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
data Phrase pch drn = Phrase { phrase_bars :: [Bar pch drn] }
  deriving (Data,Eq,Show,Typeable)



-- | Note Beaming is not captured in parsing.
--
data Bar pch drn = Bar 
    { render_info       :: LocalRenderInfo
    , bar_elements      :: [CtxElement pch drn]
    }
  deriving (Data,Eq,Show,Typeable)

-- | Note Beaming is not captured in parsing.
--
data CtxElement pch drn = Atom    (Element pch drn)
                        | Beamed  [CtxElement pch drn]
                        | Tuplet  TupletSpec            [CtxElement pch drn]
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



pushLocalRenderInfo :: LocalRenderInfo -> Phrase pch drn -> Phrase pch drn
pushLocalRenderInfo ri (Phrase bs) = Phrase $ map upd bs
  where
    upd bar = bar { render_info = ri }


sizeCtxElement :: CtxElement pch Duration -> RDuration
sizeCtxElement (Atom e)            = sizeElement e
sizeCtxElement (Beamed es)         = sum $ map sizeCtxElement es
sizeCtxElement (Tuplet {})         = error "sizeCtxElement (Tuplet {})"

sizeElement :: Element pch Duration -> RDuration
sizeElement (NoteElem (Note _ d))  = durationSize d
sizeElement (Rest d)               = durationSize d
sizeElement (Chord _ d)            = durationSize d
sizeElement (Graces {})            = 0
