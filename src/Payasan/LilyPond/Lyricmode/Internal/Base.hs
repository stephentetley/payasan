{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Lyricmode.Internal.Base
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Lyricmode for LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Lyricmode.Internal.Base
  ( 
   
    LyricSection1

  , LyLyricSection
  , LyLyricBar
  , LyLyricNoteGroup
  , LyLyricElement

  , LyLyricSection1
  , LyLyricBar1
  , LyLyricNoteGroup1
  , LyLyricElement1

  , Syllable(..)

  , inTrans

  ) where

-- import Payasan.Score.Elementary.Internal.LilyPondInTrans
import qualified Payasan.Score.Elementary.Internal.Syntax   as ELEM

import Payasan.PSC.LilyPond.Base (LyNoteLength)
import Payasan.PSC.Base.SyntaxCommon


import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data


type LyricSection1 anno    = ELEM.StdElemSection2 Syllable anno

type LyLyricSection     = ELEM.Section    Syllable LyNoteLength ()
type LyLyricBar         = ELEM.Bar        Syllable LyNoteLength ()
type LyLyricNoteGroup   = ELEM.NoteGroup  Syllable LyNoteLength ()
type LyLyricElement     = ELEM.Element    Syllable LyNoteLength ()

type LyLyricSection1 anno       = ELEM.Section    Syllable LyNoteLength anno
type LyLyricBar1 anno           = ELEM.Bar        Syllable LyNoteLength anno
type LyLyricNoteGroup1 anno     = ELEM.NoteGroup  Syllable LyNoteLength anno
type LyLyricElement1 anno       = ELEM.Element    Syllable LyNoteLength anno


data Syllable = Syllable String
  deriving (Data,Eq,Ord,Show,Typeable)




--
-- What is an extender? 
-- It cannot be an annotation as it punctuates the note list 
-- rather than annotates a word.
--
-- It has no duration - this makes is somewhat antithetical to 
-- representing it as a pitch for a Note. We don't want to 
-- confuse the LilyPond relative duration transformation or
-- confuse the output printing.
--
-- For the time being we represent it just as a String, while
-- this is not great for pattern matching etc. it prevents 
-- adding another type parameter to the syntax.



instance Pretty Syllable where
  pPrint (Syllable s)   = text s


inTrans :: ScoreInfo -> LyLyricSection1 anno -> LyricSection1 anno
inTrans _ = error "TODO" -- trafoDuration


