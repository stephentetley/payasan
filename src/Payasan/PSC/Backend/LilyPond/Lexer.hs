{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Backend.LilyPond.Lexer
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parsec Lexer combinators for LilyPond. 
--
--------------------------------------------------------------------------------

module Payasan.PSC.Backend.LilyPond.Lexer
  (
    LyParser
  , LyLexer
  , fullParseLy
  , symbol 
  , reservedOp
  , int
  , braces
  , angles
  , squares
  , lexeme
  , whiteSpace
  ) where


import Payasan.Base.Utils

import Text.Parsec.Language                           -- package: parsec
import qualified Text.Parsec.Token as P



type LyParser a         = ParsecParser a
type LyLexer            = ParsecLexer




fullParseLy :: forall a. LyParser a -> LyParser a
fullParseLy = fullInputParse whiteSpace


--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------




symbol              :: String -> LyParser String
symbol              = P.symbol lilypond_lex



reservedOp          :: String -> LyParser ()
reservedOp          = P.reservedOp lilypond_lex

int                 :: LyParser Int
int                 = fromIntegral <$> P.integer lilypond_lex


braces              :: LyParser a -> LyParser a
braces              = P.braces lilypond_lex

angles              :: LyParser a -> LyParser a
angles              = P.angles lilypond_lex

squares             :: LyParser a -> LyParser a
squares             = P.squares lilypond_lex


lexeme              :: LyParser a -> LyParser a
lexeme              = P.lexeme lilypond_lex

whiteSpace          :: LyParser ()
whiteSpace          = P.whiteSpace lilypond_lex


lilypond_lex        :: LyLexer
lilypond_lex        = P.makeTokenParser $ 
    emptyDef { P.reservedOpNames  = ["|", "/", "[", "]"]
             , P.reservedNames    = [ ]
             , P.commentLine      = "%"
             }

