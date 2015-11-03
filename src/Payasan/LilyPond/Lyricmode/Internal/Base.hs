{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Lyricmode.Internal.Base
-- Copyright   :  (c Stephen Tetley 2015
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
   
    LyricPhrase1
  , LyLyricPhrase
  , LyLyricBar
  , LyLyricNoteGroup
  , LyLyricElement

  , Syllable(..)

  , inTrans

  ) where

import Payasan.Base.Monophonic.Internal.LilyPondInTrans
import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.LilyPond.Syntax (LyNoteLength)


import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data


type LyricPhrase1 anno  = StdMonoPhrase2 Syllable anno

type LyLyricPhrase      = Phrase     Syllable LyNoteLength ()
type LyLyricBar         = Bar        Syllable LyNoteLength ()
type LyLyricNoteGroup   = NoteGroup  Syllable LyNoteLength ()
type LyLyricElement     = Element    Syllable LyNoteLength ()


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


inTrans :: ScoreInfo -> LyLyricPhrase -> LyricPhrase1 ()
inTrans _info = trafoDuration


