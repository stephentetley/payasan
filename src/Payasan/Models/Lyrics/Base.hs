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
    LyricsPhrase 
  , Stress(..)
  , outputAsLilyPond
  ) where

import qualified Payasan.LilyPond.Lyricmode.Internal.Base       as LY
import qualified Payasan.LilyPond.Lyricmode.Notelist            as LY

import Payasan.Base.Internal.LilyPond.Utils

import qualified Payasan.Base.Monophonic.Internal.Syntax        as MONO

import Payasan.Base.Internal.CommonSyntax

import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data

type LyricsPhrase = MONO.Phrase     LY.Syllable Duration Stress


-- | We need blank
--
data Stress = PRIMARY | SECONDARY | UNSTRESSED | BLANK
  deriving (Data,Eq,Ord,Show,Typeable)


stressDefs :: Doc
stressDefs = p $+$ s $+$ u
  where
    p = definition "primary"    $ markup_ $ teeny_ $ quotedText "/"
    s = definition "secondary"  $ markup_ $ teeny_ $ quotedText "//"
    u = definition "unstressed" $ markup_ $ teeny_ $ quotedText "_"


stressUse :: Stress -> Doc
stressUse PRIMARY       = above $ command "primary"
stressUse SECONDARY     = above $ command "secondary"
stressUse UNSTRESSED    = above $ command "unstressed"
stressUse BLANK         = empty


outputAsLilyPond :: ScoreInfo -> LyricsPhrase -> String
outputAsLilyPond = LY.outputAsLilyPondDU (AnnoDU { defs = stressDefs, use = stressUse })

