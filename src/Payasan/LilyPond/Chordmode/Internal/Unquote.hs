{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Chordmode.Internal.Unquote
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translate input.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Chordmode.Internal.Unquote
  ( 
    chordmode
  , translateInput      -- name problem
  ) where

import Payasan.LilyPond.Chordmode.Internal.Base
import Payasan.LilyPond.Chordmode.Internal.Parser

import Payasan.Score.Elementary.Internal.Traversals
import Payasan.Score.Elementary.Internal.Syntax

import Payasan.PSC.LilyPond.Base

import Language.Haskell.TH.Quote


chordmode :: QuasiQuoter
chordmode = QuasiQuoter
    { quoteExp = \s -> case parseChordMode s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 



translateInput :: LyChordSection -> StdChordSection
translateInput = error "TODO" -- trafoAnnos . trafoDuration

trafoAnnos :: Section LyPitch drn ChordSuffix -> Section Chord drn ()
trafoAnnos = mapPitchAnno $ \p a -> (Chord (toPitchAbs p) a, ())