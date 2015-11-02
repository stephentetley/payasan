{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Lyrics.Base
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

module Payasan.Models.Lyrics.Base
  ( 
    Stress(..)
  , lyricsScore
  ) where

import qualified Payasan.LilyPond.Lyricmode.Internal.Base       as LY
import qualified Payasan.LilyPond.Lyricmode.Internal.Output     as LY

import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import qualified Payasan.Base.Internal.BeamSyntax as BEAM
import Payasan.Base.Internal.CommonSyntax


import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data

data Stress = PRIMARY | SECONDARY | UNSTRESSED
  deriving (Data,Eq,Ord,Show,Typeable)


stressDefs :: Doc
stressDefs = p $+$ s $+$ u
  where
    p = definition "primary"    $ renderMarkup $ teeny_ (char '/')
    s = definition "secondary"  $ renderMarkup $ teeny_ (text "//")
    u = definition "unstressed" $ renderMarkup $ teeny_ (char '_')


stressUse :: Stress -> Doc
stressUse PRIMARY       = command "primary"
stressUse SECONDARY     = command "secondary"
stressUse UNSTRESSED    = command "unstressed"


lyricsScore :: ScoreInfo 
            -> BEAM.Phrase LyPitch LyNoteLength Stress
            -> BEAM.Phrase LY.Syllable LyNoteLength anno
            -> Doc
lyricsScore = LY.lyricsScoreDU (AnnoDU  { defs = stressDefs, use = stressUse })
