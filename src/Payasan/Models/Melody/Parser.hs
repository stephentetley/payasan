{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Melody.Parser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser based on ABC notation.
--
--------------------------------------------------------------------------------

module Payasan.Models.Melody.Parser
  (
    melody
  , parsetest
  ) where

import Payasan.Models.Melody.AbcSyntax
import Payasan.Models.Melody.Base ( Melody )

import Payasan.Base

import Text.Parsec                              -- package: parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P


import Language.Haskell.TH.Quote

import Control.Applicative hiding ( (<|>), many, optional )
import Control.Monad.Identity

parsetest :: String -> Either ParseError TimeSig
parsetest = runParser timeSig () ""

--------------------------------------------------------------------------------
-- Quasiquote

melody :: QuasiQuoter
melody = QuasiQuoter
    { quoteExp = \s -> case parseMelody s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


--------------------------------------------------------------------------------
-- Parser


type AbcParser a       = ParsecT String () Identity a
type AbcLexer          = P.GenTokenParser String () Identity

parseMelody :: String -> Either ParseError Melody
parseMelody = fmap fromAbcLike . parseMotif

parseMotif :: String -> Either ParseError AbcMotif
parseMotif = runParser abcMotif () ""

abcMotif :: AbcParser AbcMotif
abcMotif = AbcMotif <$> (whiteSpace *> header) <*> elements

header :: AbcParser (Maybe TimeSig)
header = Just <$> (timeSig <* reservedOp "\\\\")

timeSig :: AbcParser TimeSig
timeSig = field "M:" (lexeme timePair)
       <?> "Time Signature (M:)"

timePair :: AbcParser (Int,Int)
timePair = (,) <$> int <*> (reservedOp "/" *> int)

-- | Use @symbol@ not @reserved@ as there might not be whitespace
-- between the field identifier and the field value.
--
field :: String -> AbcParser a -> AbcParser a
field s p = symbol s *> p

elements :: AbcParser [Element]
elements = many element

element :: AbcParser Element
element = lexeme (rest <|> note)
  where
    rest = Rest <$> (char 'z' *> noteLength)
    note = Note <$> scorePitch <*> noteLength


scorePitch :: AbcParser ScorePitch
scorePitch = ScorePitch <$> accidental <*> pitchLetter <*> octave


accidental :: AbcParser Accidental
accidental = accdntl <|> return NATURAL
  where
    accdntl = choice $ 
                [ DBL_FLAT  <$ try (string "__")
                , FLAT      <$ char '_'
                , DBL_SHARP <$ try (string "^^")
                , SHARP     <$ char '^'
                ]


pitchLetter :: AbcParser PitchLetter
pitchLetter = choice $
    [ CU <$ char 'C'
    , DU <$ char 'D'
    , EU <$ char 'E'
    , FU <$ char 'F'
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

octave :: AbcParser Octave
octave = octavehi <|> octavelo <|> return DefaultOve
  where
    octavehi = (Pos . length) <$> many1 (char '\'')
    octavelo = (Neg . length) <$> many1 (char ',')



noteLength :: AbcParser NoteLength
noteLength = try (divd <|> mult) 
          <|> return DNL
  where
    divd = Divd <$> (char '/' *> int)
    mult = Mult <$> int

--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

-- parens              :: AbcParser a -> AbcParser a
-- parens              = P.parens abc_lex

int                 :: AbcParser Int
int                 = fromIntegral <$> P.decimal abc_lex


lexeme              :: AbcParser a -> AbcParser a
lexeme              = P.lexeme abc_lex

whiteSpace          :: AbcParser ()
whiteSpace          = P.whiteSpace abc_lex

symbol              :: String ->  AbcParser String
symbol              = P.symbol abc_lex

-- reserved            :: String ->  AbcParser ()
-- reserved s          = P.reserved abc_lex s
--                    <?> "reserved: " ++ s

reservedOp          :: String -> AbcParser ()
reservedOp          = P.reservedOp abc_lex


abc_lex           :: AbcLexer
abc_lex           = P.makeTokenParser abc_def
  where
    abc_def = emptyDef { P.caseSensitive = True
                       , P.reservedNames = [ "M:" ]
                       , P.reservedOpNames = [ "/", "\\\\" ]
                       }
