{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Symbolic.SimpleNoteList.Parser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Simple NoteList
--
--------------------------------------------------------------------------------

module Payasan.Symbolic.SimpleNoteList.Parser
  (
    motif
  ) where

import Payasan.Symbolic.SimpleNoteList.Base

import Text.Parsec                              -- package: parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P


import Language.Haskell.TH.Quote

import Control.Applicative hiding ( (<|>), many, optional )
import Control.Monad.Identity


--------------------------------------------------------------------------------
-- Quasiquote

motif :: QuasiQuoter
motif = QuasiQuoter
    { quoteExp = \s -> case parseMotif s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


--------------------------------------------------------------------------------
-- Parser


type ScoreParser a       = ParsecT String () Identity a
type ScoreLexer          = P.GenTokenParser String () Identity

parseMotif :: String -> Either ParseError Motif
parseMotif = runParser (fmap makeMotif elements) () ""

elements :: ScoreParser [Element]
elements = whiteSpace *> many element

element :: ScoreParser Element
element = lexeme (rest <|> note)
  where
    rest = Rest <$> (char 'z' *> noteLength)
    note = Note <$> scorePitch <*> noteLength


scorePitch :: ScoreParser ScorePitch
scorePitch = ScorePitch <$> accidental <*> pitchLetter <*> octave


accidental :: ScoreParser Accidental
accidental = accdntl <|> return NATURAL
  where
    accdntl = choice $ 
                [ DBL_FLAT  <$ try (string "__")
                , FLAT      <$ char '_'
                , DBL_SHARP <$ try (string "^^")
                , SHARP     <$ char '^'
                ]


pitchLetter :: ScoreParser PitchLetter
pitchLetter = choice $
    [ CU <$ char 'C'
    , DU <$ char 'D'
    , DU <$ char 'E'
    , DU <$ char 'F'
    , GU <$ char 'G'
    , AU <$ char 'A'
    , BU <$ char 'B'
    , CL <$ char 'c'
    , DL <$ char 'd'
    , EL <$ char 'e'
    , FL <$ char 'f'
    , GL <$ char 'g'
    , AL <$ char 'a'
    , BL <$ char 'b'
    ]

octave :: ScoreParser Octave
octave = octavehi <|> octavelo <|> return DefaultOve
  where
    octavehi = (Pos . length) <$> many1 (char '\'')
    octavelo = (Neg . length) <$> many1 (char ',')



noteLength :: ScoreParser NoteLength
noteLength = try (divd <|> mult) 
          <|> return DNL
  where
    divd = Divd <$> (char '/' *> int)
    mult = Mult <$> int

--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------


int                 :: ScoreParser Int
int                 = fromIntegral <$> P.integer score_lex


lexeme              :: ScoreParser a -> ScoreParser a
lexeme              = P.lexeme score_lex

whiteSpace          :: ScoreParser ()
whiteSpace          = P.whiteSpace score_lex

score_lex           :: ScoreLexer
score_lex           = P.makeTokenParser emptyDef
