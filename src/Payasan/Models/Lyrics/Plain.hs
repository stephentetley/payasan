{-# LANGUAGE DeriveDataTypeable         #-}
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
  ) where

import Payasan.Models.Lyrics.Base

import Payasan.LilyPond.Lyricmode.Internal.Base

import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Internal.CommonSyntax

import Payasan.Base.Duration


type LyricsAtom = NoteGroup Syllable Duration Stress


primary :: String -> Duration -> LyricsAtom
primary s d = Atom $ Note (Syllable s) d PRIMARY NO_TIE