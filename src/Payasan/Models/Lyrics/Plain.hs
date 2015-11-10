{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Lyrics.Plain
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Lyrics.
--
--------------------------------------------------------------------------------

module Payasan.Models.Lyrics.Plain
  ( 
    LyricsAtom
  , fromLyrics
  , primary
  , secondary
  , unstressed
  , hyphen
  , rest

  ) where

import Payasan.Models.Lyrics.Base

import Payasan.LilyPond.Lyricmode.Internal.Base

import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Internal.CommonSyntax

import Payasan.Base.Duration


type LyricsAtom = NoteGroup Syllable Duration Stress



fromLyrics :: [[LyricsAtom]] -> LyricsPhrase
fromLyrics xss = Phrase default_local_info $ map Bar xss



atom :: Stress -> String -> Duration -> LyricsAtom
atom str syl d = Atom $ Note (Syllable syl) d str NO_TIE

primary :: String -> Duration -> LyricsAtom
primary = atom PRIMARY

secondary :: String -> Duration -> LyricsAtom
secondary = atom SECONDARY

unstressed :: String -> Duration -> LyricsAtom
unstressed = atom UNSTRESSED

hyphen :: LyricsAtom
hyphen = Atom $ Punctuation "--"


rest :: Duration -> LyricsAtom
rest d = Atom $ Rest d