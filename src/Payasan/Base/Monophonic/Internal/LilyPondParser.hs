{-# LANGUAGE TemplateHaskell            #-}
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
    lilypond

  ) where


import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.LilyPond.Lexer
import qualified Payasan.Base.Internal.LilyPond.Parser as P

import Payasan.Base.Internal.LilyPond.Syntax ( Pitch, NoteLength )

import Text.Parsec                              -- package: parsec


import Language.Haskell.TH.Quote

import Data.Char (isSpace)


--------------------------------------------------------------------------------
-- Quasiquote

lilypond :: QuasiQuoter
lilypond = QuasiQuoter
    { quoteExp = \s -> case parseLyPhrase s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


--------------------------------------------------------------------------------
-- Parser





parseLyPhrase :: String -> Either ParseError LilyPondMonoPhrase
parseLyPhrase = runParser fullLyPhrase () ""

fullLyPhrase :: LilyPondParser LilyPondMonoPhrase
fullLyPhrase = whiteSpace *> lyPhraseK >>= step
  where 
    isTrail             = all (isSpace)
    step (ans,_,ss) 
        | isTrail ss    = return ans
        | otherwise     = fail $ "parseFail - remaining input: " ++ ss


lyPhraseK :: LilyPondParser (LilyPondMonoPhrase,SourcePos,String)
lyPhraseK = (,,) <$> phrase <*> getPosition <*> getInput


phrase :: LilyPondParser LilyPondMonoPhrase
phrase = Phrase <$> bars

bars :: LilyPondParser [Bar Pitch NoteLength]
bars = sepBy bar barline

barline :: LilyPondParser ()
barline = reservedOp "|"

bar :: LilyPondParser (Bar Pitch NoteLength)
bar = Bar default_local_info <$> ctxElements 


ctxElements :: LilyPondParser [CtxElement Pitch NoteLength]
ctxElements = whiteSpace *> many ctxElement

ctxElement :: LilyPondParser (CtxElement Pitch NoteLength)
ctxElement = tuplet <|> (Atom <$> element)


element :: LilyPondParser (Element Pitch NoteLength)
element = lexeme (rest <|> note)



note :: LilyPondParser (Element Pitch NoteLength)
note = Note <$> P.pitch <*> P.noteLength
    <?> "note"


rest :: LilyPondParser (Element Pitch NoteLength)
rest = Rest <$> (char 'z' *> P.noteLength)

tuplet :: LilyPondParser (CtxElement Pitch NoteLength)
tuplet = 
    (\spec notes -> Tuplet (P.transTupletSpec spec (length notes)) notes)
      <$> P.tupletSpec <*> braces (ctxElements)



