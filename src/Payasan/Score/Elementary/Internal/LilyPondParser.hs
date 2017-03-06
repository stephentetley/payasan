{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.LilyPondParser
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Elementary notelist using LilyPond notation. 
--
--------------------------------------------------------------------------------

module Payasan.Score.Elementary.Internal.LilyPondParser
  (

    LyParserDef (..)    -- re-export
  , parseLilyPondNoAnno 
  , parseLySection
  , pitch               -- re-export
  , noAnno              -- re-export

  ) where


import Payasan.Score.Elementary.Internal.Syntax

import Payasan.PSC.LilyPond.Lexer
import qualified Payasan.PSC.LilyPond.ExternalParser as P
import Payasan.PSC.LilyPond.ExternalParser (LyParserDef(..), pitch, noAnno)

import Payasan.PSC.Base.SyntaxCommon


import Text.Parsec                              -- package: parsec




--------------------------------------------------------------------------------
-- Parser


parseLilyPondNoAnno :: String -> Either ParseError (LyElemSection1 ())
parseLilyPondNoAnno = parseLySection parsedef
  where
    parsedef = LyParserDef { pitchParser = pitch, annoParser = noAnno }


parseLySection :: P.LyParserDef pch anno
               -> String 
               -> Either ParseError (LyElemSection2 pch anno)
parseLySection def = runParser (makeLyParser def) () ""


makeLyParser :: forall pch anno. 
                P.LyParserDef pch anno -> LyParser (LyElemSection2 pch anno)
makeLyParser def = fullParseLy section
  where
    pPitch :: LyParser pch
    pPitch = P.pitchParser def

    pAnno  :: LyParser anno
    pAnno  = P.annoParser def

    section :: LyParser (LyElemSection2 pch anno)
    section = Section "TODO" default_section_info <$> bars

    bars :: LyParser [LyElemBar2 pch anno]
    bars = sepBy bar P.barline

    bar :: LyParser (LyElemBar2 pch anno)
    bar = Bar <$> noteGroups 

    noteGroups :: LyParser [LyElemNoteGroup2 pch anno]
    noteGroups = whiteSpace *> many (ignoreSquares noteGroup)

    noteGroup :: LyParser (LyElemNoteGroup2 pch anno)
    noteGroup = tuplet <|> (Atom <$> element)

    tuplet :: LyParser (LyElemNoteGroup2 pch anno)
    tuplet = 
        (\spec notes -> Tuplet (P.makeTupletSpec spec (length notes)) notes)
            <$> P.tupletSpec <*> braces elements

    elements :: LyParser [LyElemElement2 pch anno]
    elements = whiteSpace *> many (ignoreSquares element)

    element :: LyParser (LyElemElement2 pch anno)
    element = lexeme (rest <|> note)

    note :: LyParser (LyElemElement2 pch anno)
    note = (\p d a t -> Note p d a t) 
             <$> pPitch <*> P.noteLength <*> pAnno <*> P.tie
        <?> "note"

    rest :: LyParser (LyElemElement2 pch anno)
    rest = Rest <$> (char 'r' *> P.noteLength)


ignoreSquares :: LyParser a -> LyParser a
ignoreSquares p = open *> p <* close
  where
    open  = try (symbol "[") <|> pure ""
    close = try (symbol "]") <|> pure ""


