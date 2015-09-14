{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.LilyPondQuasiquote
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Monophonic notelist using LilyPond notation. 
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.LilyPondQuasiquote
  (
    lilypond

  ) where


import Payasan.Base.Monophonic.Internal.LilyPondParser
import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.LilyPond.Parser (LyParserDef(..) , pitch)


import Text.Parsec                              -- package: parsec

import Language.Haskell.TH.Quote


--------------------------------------------------------------------------------
-- Quasiquote

lilypond :: QuasiQuoter
lilypond = QuasiQuoter
    { quoteExp = \s -> case parseLilyPond s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 



--------------------------------------------------------------------------------
-- Parser


parseLilyPond :: String -> Either ParseError LilyPondMonoPhrase
parseLilyPond = parseLyPhrase parsedef
  where
    parsedef = LyParserDef { pitchParser = pitch }
