{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.ABC.Lexer
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parsec Lexer combinators for ABC. 
--
--------------------------------------------------------------------------------

module Payasan.PSC.ABC.Lexer
  (
    ABCParser
  , ABCLexer
  , fullParseABC
  , symbol 
  , reservedOp
  , int
  , squares
  , braces
  , lexeme
  , whiteSpace
  ) where

import Payasan.PSC.Base.Utils

import Text.Parsec.Language                             -- package: parser
import qualified Text.Parsec.Token as P



type ABCParser a        = ParsecParser a
type ABCLexer           = ParsecLexer



fullParseABC :: forall a. ABCParser a -> ABCParser a
fullParseABC = fullInputParse whiteSpace


--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

symbol              :: String -> ABCParser String
symbol              = P.symbol abc_lex

reservedOp          :: String -> ABCParser ()
reservedOp          = P.reservedOp abc_lex

int                 :: ABCParser Int
int                 = fromIntegral <$> P.integer abc_lex

squares             :: ABCParser a -> ABCParser a
squares             = P.squares abc_lex

braces              :: ABCParser a -> ABCParser a
braces              = P.braces abc_lex

lexeme              :: ABCParser a -> ABCParser a
lexeme              = P.lexeme abc_lex

whiteSpace          :: ABCParser ()
whiteSpace          = P.whiteSpace abc_lex

abc_lex             :: ABCLexer
abc_lex             = P.makeTokenParser $ 
    emptyDef { P.reservedOpNames = ["|"]
             , P.reservedNames   = []
             }

