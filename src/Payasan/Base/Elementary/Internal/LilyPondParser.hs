{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.LilyPondParser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Elementary notelist using LilyPond notation. 
--
--------------------------------------------------------------------------------

module Payasan.Base.Elementary.Internal.LilyPondParser
  (

    lilypond
  , LyParserDef (..)    -- re-export
  , parseLyPhrase
  , pitch               -- re-export
  , noAnno              -- re-export

  ) where


import Payasan.Base.Elementary.Internal.Syntax

import Payasan.Base.Internal.LilyPond.Lexer
import qualified Payasan.Base.Internal.LilyPond.Parser as P
import Payasan.Base.Internal.LilyPond.Parser (LyParserDef(..), pitch, noAnno)
import Payasan.Base.Internal.CommonSyntax


import Text.Parsec                              -- package: parsec

import Language.Haskell.TH.Quote                -- package: template-haskell




--------------------------------------------------------------------------------
-- Quasiquote

lilypond :: QuasiQuoter
lilypond = QuasiQuoter
    { quoteExp = \s -> case parseLilyPondNoAnno s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


--------------------------------------------------------------------------------
-- Parser


parseLilyPondNoAnno :: String -> Either ParseError (LyElemPhrase1 ())
parseLilyPondNoAnno = parseLyPhrase parsedef
  where
    parsedef = LyParserDef { pitchParser = pitch, annoParser = noAnno }


parseLyPhrase :: P.LyParserDef pch anno
              -> String 
              -> Either ParseError (LyElemPhrase2 pch anno)
parseLyPhrase def = runParser (makeLyParser def) () ""


makeLyParser :: forall pch anno. 
                P.LyParserDef pch anno -> LyParser (LyElemPhrase2 pch anno)
makeLyParser def = fullParseLy phrase
  where
    pPitch :: LyParser pch
    pPitch = P.pitchParser def

    pAnno  :: LyParser anno
    pAnno  = P.annoParser def

    phrase :: LyParser (LyElemPhrase2 pch anno)
    phrase = Phrase default_section_info <$> bars

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

