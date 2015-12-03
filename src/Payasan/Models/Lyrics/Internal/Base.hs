{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Lyrics.Internal.Base
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

module Payasan.Models.Lyrics.Internal.Base
  (
    LyricsPhrase 
  , Stress(..)
  , outputAsLilyPond
  , lyrics

  , pStress

  ) where

import qualified Payasan.LilyPond.Lyricmode.Internal.Base       as LY
import qualified Payasan.LilyPond.Lyricmode.Internal.Parser     as LY
import qualified Payasan.LilyPond.Lyricmode.Notelist            as LY

import qualified Payasan.Base.Internal.LilyPond.Parser          as P
import Payasan.Base.Internal.LilyPond.Utils

import qualified Payasan.Base.Elementary.Internal.Syntax        as ELEM

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.Utils (ParsecParser)

import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Text.Parsec                              -- package: parsec

import Language.Haskell.TH.Quote

import Data.Data

type LyricsPhrase = ELEM.Phrase     LY.Syllable Duration Stress


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



lyrics :: QuasiQuoter
lyrics = QuasiQuoter
    { quoteExp = \s -> case parseLyrics s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 



parseLyrics :: String -> Either ParseError (LY.LyLyricPhrase1 Stress)
parseLyrics = runParser (LY.makeLyricParser pStress) () ""


pStress :: ParsecParser Stress
pStress = stress <|> return BLANK
  where
    stress  = choice [pri, sec, uns]
    pri     = PRIMARY    <$ (P.command "primary"    <|> P.command "pri")
    sec     = SECONDARY  <$ (P.command "secondary"  <|> P.command "sec")
    uns     = UNSTRESSED <$ (P.command "unstressed" <|> P.command "uns")
