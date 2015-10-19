{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.LilyPondParser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Monophonic notelist using LilyPond notation. 
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.LilyPondParser
  (

    LyParserDef (..)    -- re-export
  , parseLyPhrase
  , pitch
  , noAnno

  ) where


import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.LilyPond.Lexer
import qualified Payasan.Base.Internal.LilyPond.Parser as P
import Payasan.Base.Internal.LilyPond.Parser (LyParserDef(..), pitch, noAnno)
import Payasan.Base.Internal.CommonSyntax


import Text.Parsec                              -- package: parsec


--------------------------------------------------------------------------------
-- Parser


parseLyPhrase :: P.LyParserDef pch anno
              -> String 
              -> Either ParseError (GenLyMonoPhrase pch anno)
parseLyPhrase def = runParser (makeLyParser def) () ""


makeLyParser :: forall pch anno. 
                P.LyParserDef pch anno -> LyParser (GenLyMonoPhrase pch anno)
makeLyParser def = fullInputParse phrase
  where
    pPitch :: LyParser pch
    pPitch = P.pitchParser def

    pAnno  :: LyParser anno
    pAnno  = P.annoParser def

    phrase :: LyParser (GenLyMonoPhrase pch anno)
    phrase = Phrase default_local_info <$> bars

    bars :: LyParser [GenLyMonoBar pch anno]
    bars = sepBy bar P.barline

    bar :: LyParser (GenLyMonoBar pch anno)
    bar = Bar <$> noteGroups 

    noteGroups :: LyParser [GenLyMonoNoteGroup pch anno]
    noteGroups = whiteSpace *> many noteGroup

    noteGroup :: LyParser (GenLyMonoNoteGroup pch anno)
    noteGroup = tuplet <|> (Atom <$> element)

    tuplet :: LyParser (GenLyMonoNoteGroup pch anno)
    tuplet = 
        (\spec notes -> Tuplet (P.makeTupletSpec spec (length notes)) notes)
            <$> P.tupletSpec <*> braces (noteGroups)

    element :: LyParser (GenLyMonoElement pch anno)
    element = lexeme (rest <|> note)

    note :: LyParser (GenLyMonoElement pch anno)
    note = (\p d a t -> Note p d a t no_markup) 
             <$> pPitch <*> P.noteLength <*> pAnno <*> P.tie
        <?> "note"

    rest :: LyParser (GenLyMonoElement pch anno)
    rest = Rest <$> (char 'r' *> P.noteLength)




