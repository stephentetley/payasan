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
    parseLyPhrase

  ) where


import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.LilyPond.Lexer
import qualified Payasan.Base.Internal.LilyPond.Parser as P
import Payasan.Base.Internal.CommonSyntax


import Text.Parsec                              -- package: parsec


import Data.Char (isSpace)


--------------------------------------------------------------------------------
-- Parser


parseLyPhrase :: P.LyParserDef pch anno
              -> String 
              -> Either ParseError (GenLyMonoPhrase pch anno)
parseLyPhrase def = runParser (makeLyParser def) () ""


makeLyParser :: forall pch anno. 
                P.LyParserDef pch anno -> LyParser (GenLyMonoPhrase pch anno)
makeLyParser def = fullLyPhrase
  where
    pPitch :: LyParser pch
    pPitch = P.pitchParser def

    pAnno  :: LyParser anno
    pAnno  = P.annoParser def

    fullLyPhrase :: LyParser (GenLyMonoPhrase pch anno)
    fullLyPhrase = whiteSpace *> lyPhraseK >>= step
      where 
        isTrail             = all (isSpace)
        step (ans,_,ss) 
            | isTrail ss    = return ans
            | otherwise     = fail $ "parseFail - remaining input: " ++ ss


    lyPhraseK :: LyParser (GenLyMonoPhrase pch anno, SourcePos, String)
    lyPhraseK = (,,) <$> phrase <*> getPosition <*> getInput


    phrase :: LyParser (GenLyMonoPhrase pch anno)
    phrase = Phrase <$> bars

    bars :: LyParser [GenLyMonoBar pch anno]
    bars = sepBy bar P.barline

    bar :: LyParser (GenLyMonoBar pch anno)
    bar = Bar default_local_info <$> noteGroups 

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
    note = Note <$> pPitch <*> P.noteLength <*> pAnno
        <?> "note"

    rest :: LyParser (GenLyMonoElement pch anno)
    rest = Rest <$> (char 'z' *> P.noteLength)




