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


import Text.Parsec                              -- package: parsec


import Data.Char (isSpace)


--------------------------------------------------------------------------------
-- Parser





parseLyPhrase :: P.LyParserDef pch 
              -> String 
              -> Either ParseError (GenMonoLyPhrase pch)
parseLyPhrase def = runParser (makeLyParser def) () ""


makeLyParser :: forall pch. P.LyParserDef pch -> LyParser (GenMonoLyPhrase pch)
makeLyParser def = fullLyPhrase
  where
    pPitch :: LyParser pch
    pPitch = P.pitchParser def

    fullLyPhrase :: LyParser (GenMonoLyPhrase pch)
    fullLyPhrase = whiteSpace *> lyPhraseK >>= step
      where 
        isTrail             = all (isSpace)
        step (ans,_,ss) 
            | isTrail ss    = return ans
            | otherwise     = fail $ "parseFail - remaining input: " ++ ss


    lyPhraseK :: LyParser (GenMonoLyPhrase pch,SourcePos,String)
    lyPhraseK = (,,) <$> phrase <*> getPosition <*> getInput


    phrase :: LyParser (GenMonoLyPhrase pch)
    phrase = Phrase <$> bars

    bars :: LyParser [GenMonoLyBar pch]
    bars = sepBy bar P.barline

    bar :: LyParser (GenMonoLyBar pch)
    bar = Bar default_local_info <$> ctxElements 

    ctxElements :: LyParser [GenMonoLyCtxElement pch]
    ctxElements = whiteSpace *> many ctxElement

    ctxElement :: LyParser (GenMonoLyCtxElement pch)
    ctxElement = tuplet <|> (Atom <$> element)

    tuplet :: LyParser (GenMonoLyCtxElement pch)
    tuplet = 
        (\spec notes -> Tuplet (P.makeTupletSpec spec (length notes)) notes)
            <$> P.tupletSpec <*> braces (ctxElements)

    element :: LyParser (GenMonoLyElement pch)
    element = lexeme (rest <|> note)

    note :: LyParser (GenMonoLyElement pch)
    note = Note <$> pPitch <*> P.noteLength
        <?> "note"

    rest :: LyParser (GenMonoLyElement pch)
    rest = Rest <$> (char 'z' *> P.noteLength)




