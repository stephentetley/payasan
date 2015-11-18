{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Cadenza.Internal.Syntax
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Symbolic notelist without bars.
--
--------------------------------------------------------------------------------

module Payasan.Base.Cadenza.Internal.Syntax
  ( 

    StdCadenzaPhrase
  , StdCadenzaNoteGroup
  , StdCadenzaElement

  , StdCadenzaPhrase1
  , StdCadenzaNoteGroup1
  , StdCadenzaElement1

  , StdCadenzaPhrase2
  , StdCadenzaNoteGroup2
  , StdCadenzaElement2

  , LyCadenzaPhrase1
  , LyCadenzaNoteGroup1
  , LyCadenzaElement1

  , LyCadenzaPhrase2
  , LyCadenzaNoteGroup2
  , LyCadenzaElement2

  , ABCCadenzaPhrase
  , ABCCadenzaNoteGroup
  , ABCCadenzaElement  

  , Phrase(..)
  , NoteGroup(..)
  , Element(..)


  , pushContextInfo
  , contextInfo
  , sizeNoteGroup

  ) where

import Payasan.Base.Internal.ABC.Syntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Data.Data

--------------------------------------------------------------------------------
-- Syntax



type StdCadenzaPhrase                   = StdCadenzaPhrase1    ()
type StdCadenzaNoteGroup                = StdCadenzaNoteGroup1 ()
type StdCadenzaElement                  = StdCadenzaElement1   ()


type StdCadenzaPhrase1     anno         = Phrase    Pitch Duration anno
type StdCadenzaNoteGroup1  anno         = NoteGroup Pitch Duration anno
type StdCadenzaElement1    anno         = Element   Pitch Duration anno

type StdCadenzaPhrase2     pch anno     = Phrase    pch Duration anno
type StdCadenzaNoteGroup2  pch anno     = NoteGroup pch Duration anno
type StdCadenzaElement2    pch anno     = Element   pch Duration anno

type LyCadenzaPhrase1      anno         = LyCadenzaPhrase2     LyPitch anno
type LyCadenzaNoteGroup1   anno         = LyCadenzaNoteGroup2  LyPitch anno
type LyCadenzaElement1     anno         = LyCadenzaElement2    LyPitch anno

type LyCadenzaPhrase2      pch anno     = Phrase    pch LyNoteLength anno
type LyCadenzaNoteGroup2   pch anno     = NoteGroup pch LyNoteLength anno
type LyCadenzaElement2     pch anno     = Element   pch LyNoteLength anno


type ABCCadenzaPhrase                   = Phrase    ABCPitch ABCNoteLength ()
type ABCCadenzaNoteGroup                = NoteGroup ABCPitch ABCNoteLength ()
type ABCCadenzaElement                  = Element   ABCPitch ABCNoteLength ()




-- | Parametric on pitch so we can have the same syntax to 
-- represent scale degrees, drum notes, etc.
--
-- Parametric on duration so we can read ABC and decode duration
-- multipliers in a post-parsing phase.
--
-- LocalRenderInfo is annotated at the Phrase level - while this
-- prevents concatenation it simplifies transformation.
-- 
data Phrase pch drn anno = Phrase 
    { phrase_header     :: !LocalContextInfo
    , phrase_groups     :: [NoteGroup pch drn anno] 
    }
  deriving (Data,Eq,Show,Typeable)





-- | Beaming should be hand coded.
--
-- Tuplets are essential (even though they greatly complicate things).
--
data NoteGroup pch drn anno = 
      Atom    (Element pch drn anno)
    | Beamed  [NoteGroup pch drn anno]
    | Tuplet  TupletSpec         [NoteGroup pch drn anno]
  deriving (Data,Eq,Show,Typeable)


-- TODO - should Element follow Main syntax (include chords and 
-- graces) or Monophonic syntax (and omit them)?
--
data Element pch drn anno = 
      Note          pch drn   anno  Tie
    | Rest          drn
    | Spacer        drn
    | Skip          drn
    | Punctuation   String
  deriving (Data,Eq,Show,Typeable)



--------------------------------------------------------------------------------
-- Push RenderInfo into bars.


pushContextInfo :: LocalContextInfo 
                -> Phrase pch drn anno 
                -> Phrase pch drn anno
pushContextInfo ri (Phrase { phrase_groups = gs }) = 
    Phrase { phrase_header = ri
           , phrase_groups = gs }


contextInfo :: Phrase pch drn anno -> LocalContextInfo
contextInfo = phrase_header




sizeNoteGroup :: NoteGroup pch Duration anno -> RDuration
sizeNoteGroup (Atom e)          = sizeElement e
sizeNoteGroup (Beamed gs)       = sum $ map sizeNoteGroup gs
sizeNoteGroup (Tuplet spec es)  = tupletUnitRDuration spec (firstOf es)
  where
    firstOf (x:_)   = sizeNoteGroup x
    firstOf []      = toRDuration d_eighth

sizeElement :: Element pch Duration anno -> RDuration
sizeElement (Note _ d _ _)          = toRDuration d
sizeElement (Rest d)                = toRDuration d
sizeElement (Spacer d)              = toRDuration d
sizeElement (Skip d)                = toRDuration d
sizeElement (Punctuation {})        = 0


