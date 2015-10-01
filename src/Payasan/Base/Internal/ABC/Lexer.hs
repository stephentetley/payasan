{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.Lexer
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parsec Lexer combinators for ABC. 
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABC.Lexer
  (
    ABCParser
  , ABCLexer
  , fullInputParse
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
import Data.Char (isSpace)

type ABCParser a        = ParsecT String () Identity a
type ABCLexer           = P.GenTokenParser String () Identity



fullInputParse :: forall a. ABCParser a -> ABCParser a
fullInputParse p = whiteSpace *> parseK >>= step
  where 
    isTrail             = all (isSpace)
    step (ans,_,ss) 
        | isTrail ss    = return ans
        | otherwise     = fail $ "parseFail - remaining input: " ++ ss


    parseK :: ABCParser (a, SourcePos, String)
    parseK = (,,) <$> p <*> getPosition <*> getInput


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

