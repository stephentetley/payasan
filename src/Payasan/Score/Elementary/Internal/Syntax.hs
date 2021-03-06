{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.Syntax
-- Copyright   :  (c) Stephen Tetley 2015-2017
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

module Payasan.Score.Elementary.Internal.Syntax
  ( 

    ABCSectionQuote(..)  
  , LySectionQuote(..)

  , Section(..)
  , Bar(..)
  , NoteGroup(..)
  , Element(..)


  , emptyOf
  , pushSectionInfo
  , sectionInfo
  , sizeNoteGroup
  , sizeElement
  , updatePosNoteGroup
  , updatePosElement

  -- These may be moved...
  , Linear
  , View(..)
  , toLinear
  , fromLinear
  , viewl

  ) where

import Payasan.Score.Analysis.Common

import Payasan.PSC.ABC.Base hiding ( ABCSectionQuote(..) )
import Payasan.PSC.LilyPond.Base hiding ( LySectionQuote(..) )

import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration

import Data.Data

--------------------------------------------------------------------------------
-- Syntax





newtype ABCSectionQuote anno =
    ABCSectionQuote { getABCSectionQuote :: [Bar ABCPitch ABCNoteLength anno] } 
    deriving (Data,Eq,Show,Typeable)


newtype LySectionQuote pch anno = 
    LySectionQuote { getLySectionQuote :: [Bar pch LyNoteLength anno] } 
    deriving (Data,Eq,Show,Typeable)

-- | Parametric on pitch so we can have the same syntax to 
-- represent scale degrees, drum notes, etc.
--
-- Parametric on duration so we can read ABC and decode duration
-- multipliers in a post-parsing phase.
--
-- LocalRenderInfo is annotated at the Section level - while this
-- prevents concatenation it simplifies transformation.
-- 
data Section pch drn anno = Section 
    { section_name      :: !String
    , section_info      :: !SectionInfo
    , section_bars      :: [Bar pch drn anno] 
    }
  deriving (Data,Eq,Show,Typeable)



-- | Note Beaming is not captured in parsing.
--
data Bar pch drn anno = Bar 
    { bar_groups        :: [NoteGroup pch drn anno]
    }
  deriving (Data,Eq,Show,Typeable)




-- | Note - Beaming is added in rendering.
--
-- Tuplets seem essential - but perhaps they do not need 
-- nesting...
--
data NoteGroup pch drn anno = 
      Atom    (Element pch drn anno)
    | Tuplet  TupletSpec         [Element pch drn anno]
  deriving (Data,Eq,Show,Typeable)


-- | Note - unfortunately Spacers and Skips are interpreted
-- differently by LilyPond hence we must distinguish them.
--
data Element pch drn anno = 
      Note          pch   drn   anno  Tie
    | Rest          drn
    | Spacer        drn
    | Skip          drn
    | Punctuation   String
  deriving (Data,Eq,Show,Typeable)



--------------------------------------------------------------------------------
-- Operations etc.



emptyOf :: Section pch drn anno -> Section pch drn anno
emptyOf (Section { section_info = info
                 , section_name = name }) = 
    Section { section_name   = name
            , section_info = info
            , section_bars   = [] }


-- Push RenderInfo into bars.
--
pushSectionInfo :: SectionInfo 
                -> Section pch drn anno 
                -> Section pch drn anno
pushSectionInfo info s = s { section_info = info }


sectionInfo :: Section pch drn anno -> SectionInfo
sectionInfo = section_info


sizeNoteGroup :: NoteGroup pch Duration anno -> RatDuration
sizeNoteGroup (Atom e)          = sizeElement e
sizeNoteGroup (Tuplet spec es)  = tupletUnitRatDuration spec (firstOf es)
  where
    firstOf (e:_)   = sizeElement e
    firstOf []      = durationToRatDuration d_eighth

sizeElement :: Element pch Duration anno -> RatDuration
sizeElement (Note _ d _ _)      = durationToRatDuration d
sizeElement (Rest d)            = durationToRatDuration d
sizeElement (Spacer d)          = durationToRatDuration d
sizeElement (Skip d)            = durationToRatDuration d
sizeElement (Punctuation {})    = 0


updatePosElement :: Element pch drn anno -> Position -> Position
updatePosElement (Note {})          = incPositionIndex 1
updatePosElement (Rest {})          = incPositionIndex 1
updatePosElement (Spacer {})        = incPositionIndex 1
updatePosElement (Skip {})          = incPositionIndex 1
updatePosElement (Punctuation {})   = id

updatePosNoteGroup :: NoteGroup pch drn anno -> Position -> Position
updatePosNoteGroup (Atom e)         = updatePosElement e
updatePosNoteGroup (Tuplet _ es)    = \pos -> foldr updatePosElement pos es


--------------------------------------------------------------------------------
-- Views


-- Dont expose the constructor...
--
data Linear pch drn anno = Linear !String !SectionInfo !Position [NoteGroup pch drn anno] [Bar pch drn anno]


-- Possibly extend the (Position,Element) pair with Maybe TupletSpec
data View pch drn anno = Empty | (Position,Element pch drn anno) :< Linear pch drn anno


toLinear :: Section pch drn anno -> Linear pch drn anno
toLinear (Section name info bs) = 
   let (xs,ys) = case bs of { [] -> ([],[])
                            ; (z:zs) -> (bar_groups z,zs) }
   in Linear name info (Position 1 1) xs ys


fromLinear :: Linear pch drn anno -> Section pch drn anno
fromLinear (Linear name info _ es bs) = 
    Section { section_name  = name
            , section_info  = info
            , section_bars  = Bar es : bs }


viewl :: Linear pch drn anno -> View pch drn anno
viewl (Linear name info pos xs ys) = elements xs
  where
    elements (Atom e:es)            = (pos,e) :< Linear name info (incPositionIndex 1 pos) es ys
    elements (Tuplet spec ts:es)    = case listL ts of
      Nothing -> elements es
      Just (a,as) -> (pos,a) :< Linear name info (incPositionIndex 1 pos) (Tuplet spec as:es) ys

    elements []                     = nextbar ys

    nextbar (b:bs)                  = viewl $ Linear name info (incPositionBar 1 pos) (bar_groups b) bs
    nextbar []                      = Empty


listL :: [a] -> Maybe (a, [a])
listL []     = Nothing
listL (x:xs) = Just (x,xs)


--- 

-- TODO - indexed map?
