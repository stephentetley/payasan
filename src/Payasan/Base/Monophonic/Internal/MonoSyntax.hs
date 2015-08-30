{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monomorphic.Internal.MonoSyntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Very simple symbolic notelist with notes, rests, 
-- and triplets (no chords or triplets).
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.MonoSyntax
  ( 
    module Payasan.Base.Internal.CommonSyntax

  , MonoPhrase(..)
  , StdMonoPhrase
  , ABCMonoPhrase

  , Bar(..)
  , CtxElement(..)
  , Element(..)

  ) where

import Payasan.Base.Internal.ABCSyntax (NoteLength(..))
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Data.Data

--------------------------------------------------------------------------------
-- Syntax



-- | Parametric on pitch so we can have the same syntax to 
-- represent scale degrees, drum notes, etc.
--
-- Parametric on duration so we can read ABC and decode duration
-- multipliers in a post-parsing phase.
--
data MonoPhrase pch drn = MonoPhrase { phrase_bars :: [Bar pch drn] }
  deriving (Data,Eq,Show,Typeable)

type StdMonoPhrase = MonoPhrase Pitch Duration


type ABCMonoPhrase = MonoPhrase Pitch NoteLength



-- | Note Beaming is not captured in parsing.
--
data Bar pch drn = Bar 
    { render_info       :: RenderInfo
    , bar_elements      :: [CtxElement pch drn]
    }
  deriving (Data,Eq,Show,Typeable)




-- | Note Beaming is added in rendering.
--
data CtxElement pch drn = Atom    (Element pch drn)
                        | Tuplet  TupletSpec          [CtxElement pch drn]
  deriving (Data,Eq,Show,Typeable)


-- | TODO - if we allow ties, there is a simple duration 
-- doubling transformation (unfortunately there isn\'t a 
-- simple duration halving trafo).
--
--
data Element pch drn = Note   pch   drn
                     | Rest   drn
  deriving (Data,Eq,Show,Typeable)



