{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.ABCParser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Elementary notelist using ABC notation. 
--
--------------------------------------------------------------------------------

module Payasan.Base.Elementary.Internal.ABCParser
  (
    abc
  ) where

import Payasan.Base.Elementary.Internal.Syntax

import Payasan.Base.Internal.ABC.Lexer
import qualified Payasan.Base.Internal.ABC.Parser as P
import Payasan.Base.Internal.ABC.Syntax (ABCPitch)

import Payasan.Base.Internal.CommonSyntax


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



parseABCPhrase :: String -> Either ParseError ABCElemPhrase
parseABCPhrase = runParser fullABCPhrase () ""




fullABCPhrase :: ABCParser ABCElemPhrase
fullABCPhrase = whiteSpace *> abcPhraseK >>= step
  where 
    isTrail             = all (isSpace)
    step (ans,_,ss) 
        | isTrail ss    = return ans
        | otherwise     = fail $ "parseFail - remaining input: " ++ ss


abcPhraseK :: ABCParser (ABCElemPhrase,SourcePos,String)
abcPhraseK = (,,) <$> phrase <*> getPosition <*> getInput

phrase :: ABCParser ABCElemPhrase 
phrase = Phrase default_section_info <$> bars

bars :: ABCParser [ABCElemBar]
bars = sepBy bar barline

barline :: ABCParser ()
barline = reservedOp "|"

bar :: ABCParser ABCElemBar
bar = Bar <$> noteGroups 


noteGroups :: ABCParser [ABCElemNoteGroup]
noteGroups = whiteSpace *> many noteGroup


noteGroup :: ABCParser ABCElemNoteGroup
noteGroup = tuplet <|> (Atom <$> element)

element :: ABCParser ABCElemElement
element = lexeme (rest <|> note)

rest :: ABCParser ABCElemElement
rest = Rest <$> (char 'z' *> P.noteLength)

note :: ABCParser ABCElemElement
note = (\p d t -> Note p d () t) <$> pitch <*> P.noteLength <*> P.tie
    <?> "note"


-- Cannot use parsecs count as ABC counts /deep leaves/.
--
tuplet :: ABCParser ABCElemNoteGroup
tuplet = do 
   spec   <- P.tupletSpec
   notes  <- countedElements (tuplet_len spec)
   return $ Tuplet spec notes

countedElements :: Int -> ABCParser [ABCElemElement]
countedElements n 
    | n > 0       = do { e  <- element
                       ; es <- countedElements (n - 1)
                       ; return $ e:es
                       }
    | otherwise   = return []
                       

pitch :: ABCParser ABCPitch
pitch = P.pitch

