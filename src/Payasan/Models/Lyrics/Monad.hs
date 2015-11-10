{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Lyrics.Monad
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

module Payasan.Models.Lyrics.Monad
  ( 
    Lyrics
  , fromLyricsM
  , tell
  , rest

  ) where

import Payasan.Models.Lyrics.Base

import Payasan.LilyPond.Lyricmode.Internal.Base

import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.Utils

import Payasan.Base.Duration

import Data.List

type LyricsAtom = NoteGroup Syllable Duration Stress

newtype Lyrics a = Lyrics { getLyrics :: (H LyricsAtom, a) }

instance Functor Lyrics where
  fmap f ma = Lyrics $ let (w,a) = getLyrics ma in (w,f a)

instance Applicative Lyrics where
  pure a    = Lyrics $ (emptyH, a)
  mf <*> ma = Lyrics $ let (w1,f) = getLyrics mf
                           (w2,a) = getLyrics ma
                       in (w1 `appendH` w2, f a)


instance Monad Lyrics where
  return    = pure
  ma >>= k  = Lyrics $ let (w1,a) = getLyrics ma
                           (w2,b) = getLyrics (k a)
                       in (w1 `appendH` w2, b)


instance Monoid a => Monoid (Lyrics a) where
  mempty          = Lyrics $ (emptyH, mempty)
  ma `mappend` mb = Lyrics $ let (w1,a) = getLyrics ma
                                 (w2,b) = getLyrics mb
                             in (w1 `appendH` w2, a `mappend` b)


fromLyricsM :: Lyrics a -> LyricsPhrase
fromLyricsM ma = let (w,_) = getLyrics ma in Phrase default_local_info [Bar $ toListH w]

tell_ :: String -> Duration -> Lyrics ()
tell_ syl d = Lyrics $ (wrapH atom, ())
  where 
    atom = Atom $ Note (Syllable syl) d UNSTRESSED NO_TIE


hyphen_ :: Lyrics ()
hyphen_ = Lyrics $ (wrapH atom, ())
  where
    atom = Atom $ Punctuation "--"


tell :: String -> Duration -> Lyrics ()
tell syl d 
   | null syl           = rest d
   | isSuffixOf "-" syl = tell_ (init syl) d >> hyphen_
   | otherwise          = tell_ syl d


rest :: Duration -> Lyrics ()
rest d = Lyrics $ (wrapH atom, ())
  where
    atom = Atom $ Rest d

