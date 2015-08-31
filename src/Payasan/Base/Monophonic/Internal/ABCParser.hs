{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.ABCParser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Monophonic notelist using ABC notation. 
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.ABCParser
  (
    abc
  ) where

import Payasan.Base.Monophonic.Internal.MonoSyntax

import Payasan.Base.Internal.ABCLexer
import qualified Payasan.Base.Internal.ABCParser as P
import Payasan.Base.Internal.ABCSyntax (NoteLength(..))
import Payasan.Base.Internal.ABCUtils ( toPitch )

import Payasan.Base.Pitch

import Text.Parsec                              -- package: parsec


import Language.Haskell.TH.Quote

import Data.Char (isSpace)

--------------------------------------------------------------------------------
-- Quasiquote

-- Note - unfortunately we can\'t parameterize the quasiquoter
-- (e.g. with default note length)


abc :: QuasiQuoter
abc = QuasiQuoter
    { quoteExp = \s -> case parseABCPhrase s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


--------------------------------------------------------------------------------
-- Parser



parseABCPhrase :: String -> Either ParseError ABCMonoPhrase
parseABCPhrase = runParser fullABCPhrase () ""




fullABCPhrase :: ABCParser ABCMonoPhrase
fullABCPhrase = whiteSpace *> abcPhraseK >>= step
  where 
    isTrail             = all (isSpace)
    step (ans,_,ss) 
        | isTrail ss    = return ans
        | otherwise     = fail $ "parseFail - remaining input: " ++ ss


abcPhraseK :: ABCParser (ABCMonoPhrase,SourcePos,String)
abcPhraseK = (,,) <$> phrase <*> getPosition <*> getInput

phrase :: ABCParser ABCMonoPhrase 
phrase = MonoPhrase <$> bars

bars :: ABCParser [Bar Pitch NoteLength]
bars = sepBy bar barline

barline :: ABCParser ()
barline = reservedOp "|"

bar :: ABCParser (Bar Pitch NoteLength)
bar = Bar default_local_info <$> ctxElements 


ctxElements :: ABCParser [CtxElement Pitch NoteLength]
ctxElements = whiteSpace *> many ctxElement


ctxElement :: ABCParser (CtxElement Pitch NoteLength)
ctxElement = tuplet <|> (Atom <$> element)

element :: ABCParser (Element Pitch NoteLength)
element = lexeme (rest <|> note)

rest :: ABCParser (Element Pitch NoteLength)
rest = Rest <$> (char 'z' *> P.noteLength)

note :: ABCParser (Element Pitch NoteLength)
note = Note <$> pitch <*> P.noteLength
    <?> "note"



-- Cannot use parsecs count as ABC counts /deep leaves/.
--
tuplet :: ABCParser (CtxElement Pitch NoteLength)
tuplet = do 
   spec   <- P.tupletSpec
   notes  <- countedCtxElements (tuplet_len spec)
   return $ Tuplet spec notes

countedCtxElements :: Int -> ABCParser [CtxElement Pitch NoteLength]
countedCtxElements n 
    | n > 0       = do { e  <- ctxElement
                       ; es <- countedCtxElements (n - elementSize e)
                       ; return $ e:es
                       }
    | otherwise   = return []
                       

pitch :: ABCParser Pitch
pitch = toPitch <$> P.pitch


--------------------------------------------------------------------------------
-- Helpers

elementSize :: CtxElement pch drn -> Int
elementSize (Tuplet spec _) = tuplet_len spec
elementSize _               = 1
