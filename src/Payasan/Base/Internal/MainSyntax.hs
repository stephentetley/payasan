{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.MainSyntax
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
-- Parametric on Duration and Pitch.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.MainSyntax
  ( 
    module Payasan.Base.Internal.CommonSyntax
  
  , Phrase(..)
  , Bar(..)
  , CtxElement(..)
  , Element(..)
  , Note(..)

  ) where


import Payasan.Base.Internal.CommonSyntax

import Data.Data



--------------------------------------------------------------------------------
-- Syntax


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
                        | Tuplet  TupletSpec        [CtxElement pch drn]
  deriving (Data,Eq,Show,Typeable)


-- | Note is should be quite easy to add ties (as write-only)
-- to get long notes after beaming.
--
-- See old Neume code. 
--
data Element pch drn = NoteElem   (Note pch drn)
                     | Rest       drn
                     | Chord      [pch]             drn
                     | Graces     [Note pch drn]
  deriving (Data,Eq,Show,Typeable)


data Note pch drn = Note pch drn
  deriving (Data,Eq,Show,Typeable)

