{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.Lexer
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

module Payasan.Base.Internal.LilyPond.Lexer
  (
    LyParser
  , LyLexer
  , fullParseLy
  , symbol 
  , reservedOp
  , int
  , braces
  , angles
  , lexeme
  , whiteSpace
  ) where


import Payasan.Base.Internal.Utils

import Text.Parsec                              -- package: parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P



type LyParser a         = ParsecParser a
type LyLexer            = ParsecLexer




fullParseLy :: forall a. LyParser a -> LyParser a
fullParseLy = fullInputParse whiteSpace


--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------


-- TODO - this is too underhand...
--
whiteSpace          :: LyParser ()
whiteSpace          = whiteSpace1 <|> reservedOp "[" <|> reservedOp "]"


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

lexeme              :: LyParser a -> LyParser a
lexeme              = P.lexeme lilypond_lex

whiteSpace1         :: LyParser ()
whiteSpace1         = P.whiteSpace lilypond_lex


lilypond_lex        :: LyLexer
lilypond_lex        = P.makeTokenParser $ 
    emptyDef { P.reservedOpNames  = ["|", "/", "[", "]"]
             , P.reservedNames    = [ ]
             , P.commentLine      = "%"
             }

