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

  , LyLyricPhrase1
  , LyLyricBar1
  , LyLyricNoteGroup1
  , LyLyricElement1

  , Syllable(..)

  , inTrans

  ) where

import Payasan.Base.Monophonic.Internal.LilyPondInTrans
import qualified Payasan.Base.Monophonic.Internal.Syntax    as MONO

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.LilyPond.Syntax (LyNoteLength)


import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data


type LyricPhrase1 anno  = MONO.StdMonoPhrase2 Syllable anno

type LyLyricPhrase      = MONO.Phrase     Syllable LyNoteLength ()
type LyLyricBar         = MONO.Bar        Syllable LyNoteLength ()
type LyLyricNoteGroup   = MONO.NoteGroup  Syllable LyNoteLength ()
type LyLyricElement     = MONO.Element    Syllable LyNoteLength ()

type LyLyricPhrase1 anno        = MONO.Phrase     Syllable LyNoteLength anno
type LyLyricBar1 anno           = MONO.Bar        Syllable LyNoteLength anno
type LyLyricNoteGroup1 anno     = MONO.NoteGroup  Syllable LyNoteLength anno
type LyLyricElement1 anno       = MONO.Element    Syllable LyNoteLength anno


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


inTrans :: ScoreInfo -> LyLyricPhrase1 anno -> LyricPhrase1 anno
inTrans _info = trafoDuration


