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

import Payasan.PSC.LilyPond.Base
import Payasan.PSC.LilyPond.Lexer
import qualified Payasan.PSC.LilyPond.ExternalParser as P
import Payasan.PSC.LilyPond.ExternalParser (LyParserDef(..), pitch, noAnno)

import Payasan.PSC.Base.SyntaxCommon


import Text.Parsec                              -- package: parsec




--------------------------------------------------------------------------------
-- Parser


parseLilyPondNoAnno :: String -> Either ParseError (Section LyPitch LyNoteLength ())
parseLilyPondNoAnno = parseLySection parsedef
  where
    parsedef = LyParserDef { pitchParser = pitch, annoParser = noAnno }


parseLySection :: P.LyParserDef pch anno
               -> String 
               -> Either ParseError (Section pch LyNoteLength anno)
parseLySection def = runParser (makeLyParser def) () ""


makeLyParser :: forall pch anno. 
                P.LyParserDef pch anno -> LyParser (Section pch LyNoteLength anno)
makeLyParser def = fullParseLy section
  where
    pPitch :: LyParser pch
    pPitch = P.pitchParser def

    pAnno  :: LyParser anno
    pAnno  = P.annoParser def

    section :: LyParser (Section pch LyNoteLength anno)
    section = Section "TODO" default_section_info <$> bars

    bars :: LyParser [Bar pch LyNoteLength anno]
    bars = sepBy bar P.barline

    bar :: LyParser (Bar pch LyNoteLength anno)
    bar = Bar <$> noteGroups 

    noteGroups :: LyParser [NoteGroup pch LyNoteLength anno]
    noteGroups = whiteSpace *> many (ignoreSquares noteGroup)

    noteGroup :: LyParser (NoteGroup pch LyNoteLength anno)
    noteGroup = tuplet <|> (Atom <$> element)

    tuplet :: LyParser (NoteGroup pch LyNoteLength anno)
    tuplet = 
        (\spec notes -> Tuplet (P.makeTupletSpec spec (length notes)) notes)
            <$> P.tupletSpec <*> braces elements

    elements :: LyParser [Element pch LyNoteLength anno]
    elements = whiteSpace *> many (ignoreSquares element)

    element :: LyParser (Element pch LyNoteLength anno)
    element = lexeme (rest <|> note)

    note :: LyParser (Element pch LyNoteLength anno)
    note = (\p d a t -> Note p d a t) 
             <$> pPitch <*> P.noteLength <*> pAnno <*> P.tie
        <?> "note"

    rest :: LyParser (Element pch LyNoteLength anno)
    rest = Rest <$> (char 'r' *> P.noteLength)


ignoreSquares :: LyParser a -> LyParser a
ignoreSquares p = open *> p <* close
  where
    open  = try (symbol "[") <|> pure ""
    close = try (symbol "]") <|> pure ""


