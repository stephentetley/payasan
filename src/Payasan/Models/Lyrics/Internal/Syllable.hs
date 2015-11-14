{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Lyrics.Internal.Syllable
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Lyrics without durations.
--
--------------------------------------------------------------------------------

module Payasan.Models.Lyrics.Internal.Syllable
  (
    Lyrics
  , Syllable(..)
  , Hyphen(..)

  , lyrics

  ) where


import Payasan.Models.Lyrics.Internal.Base hiding (lyrics)

import Payasan.Base.Internal.Utils





import Text.Parsec                              -- package: parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token                              as P

import Language.Haskell.TH.Quote

import Data.Data





type Lyrics = [Syllable]


data Syllable = Syllable String Stress Hyphen
  deriving (Data,Eq,Ord,Show,Typeable)


data Hyphen = NO_HYPHEN | HYPHEN
  deriving (Data,Eq,Ord,Show,Typeable)


lyrics :: QuasiQuoter
lyrics = QuasiQuoter
    { quoteExp = \s -> case parseLyrics s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


parseLyrics :: String -> Either ParseError Lyrics
parseLyrics = runParser (fullInputParse whiteSpace p) () ""
  where
    p = many (lexeme syllable)

syllable :: ParsecParser Syllable
syllable = Syllable <$> many1 (letter <|> oneOf "!?.,'")
                    <*> pStress
                    <*> hyphen


hyphen :: ParsecParser Hyphen
hyphen =  try dash
      <|> pure NO_HYPHEN
  where
    dash = HYPHEN <$ symbol "-"



symbol              :: String -> ParsecParser String
symbol              = P.symbol syllable_lex


lexeme              :: ParsecParser a -> ParsecParser a
lexeme              = P.lexeme syllable_lex


whiteSpace          :: ParsecParser ()
whiteSpace          = P.whiteSpace syllable_lex

syllable_lex        :: ParsecLexer
syllable_lex        = P.makeTokenParser $ 
    emptyDef { P.reservedOpNames  = ["-"]
             , P.reservedNames    = []
             , P.commentLine      = "%"
             }

