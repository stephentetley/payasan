{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABCLexer
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Monophonic notelist using ABC notation. 
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABCLexer
  (
    ABCParser
  , ABCLexer
  , symbol 
  , reservedOp
  , int
  , squares
  , braces
  , lexeme
  , whiteSpace
  ) where


import Text.Parsec                              -- package: parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P


import Control.Monad.Identity


type ABCParser a         = ParsecT String () Identity a
type ABCLexer            = P.GenTokenParser String () Identity


--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

symbol              :: String -> ABCParser String
symbol              = P.symbol score_lex

reservedOp          :: String -> ABCParser ()
reservedOp          = P.reservedOp score_lex

int                 :: ABCParser Int
int                 = fromIntegral <$> P.integer score_lex

squares             :: ABCParser a -> ABCParser a
squares             = P.squares score_lex

braces              :: ABCParser a -> ABCParser a
braces              = P.braces score_lex

lexeme              :: ABCParser a -> ABCParser a
lexeme              = P.lexeme score_lex

whiteSpace          :: ABCParser ()
whiteSpace          = P.whiteSpace score_lex

score_lex           :: ABCLexer
score_lex           = P.makeTokenParser $ 
    emptyDef { P.reservedOpNames = ["|"]
             , P.reservedNames   = []
             }

