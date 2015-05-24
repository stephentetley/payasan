{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Djembe.Parser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Djembe
--
--------------------------------------------------------------------------------

module Payasan.Models.Djembe.Parser
  ( 
    djembe
  ) where

import Payasan.Models.Djembe.Base


import Text.Parsec                              -- package: parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P


import Language.Haskell.TH.Quote

import Control.Applicative hiding ( (<|>), many, optional )
import Control.Monad.Identity

type DjembeParser a     = ParsecT String () Identity a
type DjembeLexer        = P.GenTokenParser String () Identity



--------------------------------------------------------------------------------
-- Quasiquote

djembe :: QuasiQuoter
djembe = QuasiQuoter
    { quoteExp = \s -> case parsePattern s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 



-- | Probably best to preprocess removing breaking symbols (//)
-- which have no meaning.
--
preprocess :: String -> String
preprocess []           = []
preprocess ('/':'/':xs) = preprocess xs
preprocess (x:xs)       = x : preprocess xs



parsePattern :: String -> Either ParseError DjembePattern
parsePattern = runParser patternTop () "" . preprocess


patternTop :: DjembeParser DjembePattern
patternTop = whiteSpace *> djembePattern

djembePattern :: DjembeParser DjembePattern
djembePattern = makeDjembePattern <$> many wrapper


wrapper :: DjembeParser Wrapper
wrapper = choice [ swing, flam, duplet, triplet, single ]

single :: DjembeParser Wrapper
single = One <$> note


swing :: DjembeParser Wrapper
swing = Swing <$> (reserved "swing" *> braces note)


flam :: DjembeParser Wrapper
flam = grouped "flam" $ 
    Flam <$> note <*> note

duplet :: DjembeParser Wrapper
duplet = grouped "duplet" $ 
    Duplet <$> note <*> note

triplet :: DjembeParser Wrapper
triplet = grouped "triplet" $ 
    Triplet <$> note <*> note <*> note 



note :: DjembeParser Note
note = chordOrStroke <|> rest


-- accent is a common prefix
chordOrStroke :: DjembeParser Note
chordOrStroke = accent >>= \ac -> (chord ac <|> stroke ac)


chord :: Accent -> DjembeParser Note
chord ac = C ac <$> angles (many1 identifier)

stroke :: Accent -> DjembeParser Note
stroke ac = N ac <$> identifier

rest :: DjembeParser Note
rest = R <$ reservedOp "."


accent :: DjembeParser Accent
accent = try a <|> b
  where
    a = ACCENT <$ reservedOp ">" 
    b = return NO_ACCENT

grouped :: String -> DjembeParser a -> DjembeParser a
grouped kwd p = reserved kwd *> braces p 

--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

identifier          :: DjembeParser Identifier
identifier          = Identifier <$> P.identifier djembe_lex

reserved            :: String -> DjembeParser ()
reserved s          = P.reserved djembe_lex s

reservedOp          :: String -> DjembeParser ()
reservedOp          = P.reservedOp djembe_lex

angles              :: DjembeParser a -> DjembeParser a
angles              = P.angles djembe_lex

braces              :: DjembeParser a -> DjembeParser a
braces              = P.braces djembe_lex

whiteSpace          :: DjembeParser ()
whiteSpace          = P.whiteSpace djembe_lex



--------------------------------------------------------------------------------
-- Lang def
--------------------------------------------------------------------------------

djembe_lex           :: DjembeLexer
djembe_lex           = P.makeTokenParser syntax_def


syntax_def :: LanguageDef st
syntax_def = emptyDef
    { P.identStart        = letter
    , P.identLetter       = alphaNum
    , P.reservedOpNames   = [ ".", ">" ]  
    , P.reservedNames     = [ "flam", "swing", "duplet", "triplet" ] 
    , P.caseSensitive     = True
    }

