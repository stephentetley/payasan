{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.Quasiquote
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Quasiquoter for subset of LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.Quasiquote
  (
    lilypond

  ) where


import Payasan.Base.Internal.LilyPond.Parser
import Payasan.Base.Internal.LilyPond.Syntax

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


parseLilyPond :: String -> Either ParseError (LyPhrase1 ())
parseLilyPond = parseLyPhrase parsedef
  where
    parsedef = LyParserDef { pitchParser = pitch, annoParser = noAnno }

