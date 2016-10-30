{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Lyrics.Internal.Plain
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Lyrics.
--
--------------------------------------------------------------------------------

module Payasan.Models.Lyrics.Internal.Plain
  ( 
    LyricsAtom
  , fromLyrics
  , primary
  , secondary
  , unstressed
  , hyphen
  , rest

  ) where

import Payasan.Models.Lyrics.Internal.Base

import Payasan.LilyPond.Lyricmode.Internal.Base

import Payasan.Score.Elementary.Internal.Syntax
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration


type LyricsAtom = NoteGroup Syllable Duration Stress



fromLyrics :: [[LyricsAtom]] -> LyricsPart
fromLyrics xss = Part default_section_info $ map Bar xss



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