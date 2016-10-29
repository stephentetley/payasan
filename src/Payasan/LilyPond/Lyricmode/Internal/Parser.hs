{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Lyricmode.Internal.Parser
-- Copyright   :  (c) Stephen Tetley 2015-2016
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
  , makeLyricParser

  ) where

import Payasan.LilyPond.Lyricmode.Internal.Base
import Payasan.Score.Elementary.Internal.Syntax


import Payasan.PSC.Backend.LilyPond.Lexer
import qualified Payasan.PSC.Backend.LilyPond.Parser as P

import Payasan.Base.Internal.SyntaxCommon


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



parseLyricMode :: String -> Either ParseError LyLyricPart
parseLyricMode = runParser (makeLyricParser P.noAnno) () ""



-- This parser is so different from LilyPond that it might be better 
-- to code it from scratch.


makeLyricParser :: forall anno. LyParser anno -> LyParser (LyLyricPart1 anno)
makeLyricParser pAnno = fullParseLy part
  where
    part :: LyParser (LyLyricPart1 anno)
    part = Part default_section_info <$> bars

    bars :: LyParser [LyLyricBar1 anno]
    bars = sepBy bar P.barline

    bar :: LyParser (LyLyricBar1 anno)
    bar = Bar <$> many atom

    atom :: LyParser (LyLyricNoteGroup1 anno)
    atom = Atom <$> (syllableNote <|> skip <|> punctuation)

    -- Use __ for extenders, _ is not recognized because we manually
    -- specify lyric duration in the output.
    --
    punctuation :: LyParser (LyLyricElement1 anno)
    punctuation = doubleHyphen <|> doubleUnder
      where
        doubleHyphen = lexeme (Punctuation <$> symbol "--")
        doubleUnder  = lexeme (Punctuation <$> symbol "__")

    syllableNote :: LyParser (LyLyricElement1 anno)
    syllableNote = (\p d a -> Note p d a NO_TIE) <$> syllable <*> P.noteLength <*> pAnno

    syllable :: LyParser Syllable
    syllable = Syllable <$> many1 (letter <|> oneOf "?!.,'")

    -- \skip is symbol for rest...
    skip :: LyParser (LyLyricElement1 anno)
    skip = Skip <$> (P.command "skip" *> P.noteLength)

