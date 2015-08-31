{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPondLexer
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

module Payasan.Base.Internal.LilyPondLexer
  (
    LilyPondParser
  , LilyPondLexer
  , symbol 
  , reserved
  , reservedOp
  , int
  , braces
  , lexeme
  , whiteSpace
  ) where


import Text.Parsec                              -- package: parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P


import Control.Monad.Identity


type LilyPondParser a   = ParsecT String () Identity a
type LilyPondLexer      = P.GenTokenParser String () Identity


--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

whiteSpace          :: LilyPondParser ()
whiteSpace          = whiteSpace1 <|> reservedOp "[" <|> reservedOp "]"


symbol              :: String -> LilyPondParser String
symbol              = P.symbol lilypond_lex

reserved            :: String -> LilyPondParser ()
reserved            = P.reserved lilypond_lex

reservedOp          :: String -> LilyPondParser ()
reservedOp          = P.reservedOp lilypond_lex

int                 :: LilyPondParser Int
int                 = fromIntegral <$> P.integer lilypond_lex


braces              :: LilyPondParser a -> LilyPondParser a
braces              = P.braces lilypond_lex

lexeme              :: LilyPondParser a -> LilyPondParser a
lexeme              = P.lexeme lilypond_lex

whiteSpace1         :: LilyPondParser ()
whiteSpace1         = P.whiteSpace lilypond_lex

lilypond_lex        :: LilyPondLexer
lilypond_lex        = P.makeTokenParser $ 
    emptyDef { P.reservedOpNames  = ["|", "/", "[", "]"]
             , P.reservedNames    = [ "\\maxima", "\\longa", "\\breve"
                                    , "\\tuplet"
                                    ]
             }

