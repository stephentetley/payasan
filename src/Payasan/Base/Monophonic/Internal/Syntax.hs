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

  , Phrase(..)
  , StdMonoPhrase
  , ABCMonoPhrase
  , LilyPondMonoPhrase

  , Bar(..)
  , CtxElement(..)
  , Element(..)

  , pushLocalRenderInfo
  , sizeCtxElement

  ) where

import qualified Payasan.Base.Internal.ABC.Syntax as ABC
import Payasan.Base.Internal.CommonSyntax
import qualified Payasan.Base.Internal.LilyPond.Syntax as LY
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
data Phrase pch drn = Phrase { phrase_bars :: [Bar pch drn] }
  deriving (Data,Eq,Show,Typeable)

type StdMonoPhrase          = Phrase Pitch Duration

type ABCMonoPhrase          = Phrase ABC.Pitch ABC.NoteLength
type LilyPondMonoPhrase     = Phrase LY.Pitch  LY.NoteLength



-- | Note Beaming is not captured in parsing.
--
data Bar pch drn = Bar 
    { bar_header        :: LocalRenderInfo
    , bar_elements      :: [CtxElement pch drn]
    }
  deriving (Data,Eq,Show,Typeable)




-- | Note Beaming is added in rendering.
--
-- Tuplets seem essential 
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



--------------------------------------------------------------------------------
-- Push RenderInfo into bars.


pushLocalRenderInfo :: LocalRenderInfo -> Phrase pch drn -> Phrase pch drn
pushLocalRenderInfo ri (Phrase bs) = Phrase $ map upd bs
  where
    upd bar = bar { bar_header = ri }



sizeCtxElement :: CtxElement pch Duration -> RDuration
sizeCtxElement (Atom e)            = sizeElement e
sizeCtxElement (Tuplet {})         = error "sizeCtxElement (Tuplet {})"

sizeElement :: Element pch Duration -> RDuration
sizeElement (Note _ d)          = durationSize d
sizeElement (Rest d)            = durationSize d
