{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Lyricmode.Internal.Parser
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Lyricmode parser.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Lyricmode.Internal.Parser
  ( 
   
    lyricmode

  ) where

import Payasan.LilyPond.Lyricmode.Internal.Base
import Payasan.Base.Monophonic.Internal.Syntax


import Payasan.Base.Internal.LilyPond.Lexer
import qualified Payasan.Base.Internal.LilyPond.Parser as P

import Payasan.Base.Internal.CommonSyntax


import Text.Parsec                              -- package: parsec

import Language.Haskell.TH.Quote


lyricmode :: QuasiQuoter
lyricmode = QuasiQuoter
    { quoteExp = \s -> case parseLyricMode s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 



parseLyricMode :: String -> Either ParseError LyLyricPhrase
parseLyricMode = runParser (fullInputParse phrase) () ""



phrase :: LyParser LyLyricPhrase
phrase = Phrase default_local_info <$> bars

bars :: LyParser [LyLyricBar]
bars = sepBy bar P.barline

bar :: LyParser LyLyricBar
bar = Bar <$> many atom


atom :: LyParser LyLyricNoteGroup
atom = Atom <$> (syllableNote <|> skip <|> punctuation)

-- \skip is symbol for rest...

-- This parser is so different from LilyPond that it might be better 
-- to code it from scratch.

punctuation :: LyParser LyLyricElement
punctuation = doubleHyphen <|> pUscore
  where
    doubleHyphen :: LyParser LyLyricElement
    doubleHyphen = lexeme (Punctuation <$> symbol "--")
    pUscore      = lexeme (char '_' >> uscoreK)
    uscoreK      = (Punctuation "__" <$ char '_') <|> (pure $ Punctuation "_")

syllableNote :: LyParser LyLyricElement
syllableNote = (\p d -> Note p d ()) <$> syllable <*> P.noteLength

syllable :: LyParser Syllable
syllable = Syllable <$> many1 (letter <|> oneOf ".,")

skip :: LyParser LyLyricElement
skip = Rest <$> (symbol "\\skip" *> P.noteLength)
