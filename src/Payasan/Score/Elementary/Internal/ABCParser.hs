{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.ABCParser
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Elementary notelist using ABC notation. 
--
--------------------------------------------------------------------------------

module Payasan.Score.Elementary.Internal.ABCParser
  (
    parseABCSection
  ) where

import Payasan.Score.Elementary.Internal.Syntax

import Payasan.PSC.ABC.Base
import qualified Payasan.PSC.ABC.ExternalParser as P
import Payasan.PSC.ABC.Lexer

import Payasan.PSC.Base.SyntaxCommon


import Text.Parsec                              -- package: parsec



import Data.Char (isSpace)


--------------------------------------------------------------------------------
-- Parser



parseABCSection :: String -> Either ParseError (Section ABCPitch ABCNoteLength ())
parseABCSection = runParser fullABCSection () ""




fullABCSection :: ABCParser (Section ABCPitch ABCNoteLength ())
fullABCSection = whiteSpace *> abcSectionK >>= step
  where 
    isTrail             = all (isSpace)
    step (ans,_,ss) 
        | isTrail ss    = return ans
        | otherwise     = fail $ "parseFail - remaining input: " ++ ss


abcSectionK :: ABCParser (Section ABCPitch ABCNoteLength (),SourcePos,String)
abcSectionK = (,,) <$> section <*> getPosition <*> getInput

section :: ABCParser (Section ABCPitch ABCNoteLength ())  
section = Section "TODO" default_section_info <$> bars

bars :: ABCParser [Bar ABCPitch ABCNoteLength ()]
bars = sepBy bar barline

barline :: ABCParser ()
barline = reservedOp "|"

bar :: ABCParser (Bar ABCPitch ABCNoteLength ())
bar = Bar <$> noteGroups 


noteGroups :: ABCParser [NoteGroup ABCPitch ABCNoteLength ()]
noteGroups = whiteSpace *> many noteGroup


noteGroup :: ABCParser (NoteGroup ABCPitch ABCNoteLength ())
noteGroup = tuplet <|> (Atom <$> element)

element :: ABCParser (Element ABCPitch ABCNoteLength ())
element = lexeme (rest <|> note)

rest :: ABCParser (Element ABCPitch ABCNoteLength ())
rest = Rest <$> (char 'z' *> P.noteLength)

note :: ABCParser (Element ABCPitch ABCNoteLength ())
note = (\p d t -> Note p d () t) <$> pitch <*> P.noteLength <*> P.tie
    <?> "note"


-- Cannot use parsecs count as ABC counts /deep leaves/.
--
tuplet :: ABCParser (NoteGroup ABCPitch ABCNoteLength ())
tuplet = do 
   spec   <- P.tupletSpec
   notes  <- countedElements (tuplet_len spec)
   return $ Tuplet spec notes

countedElements :: Int -> ABCParser [Element ABCPitch ABCNoteLength ()]
countedElements n 
    | n > 0       = do { e  <- element
                       ; es <- countedElements (n - 1)
                       ; return $ e:es
                       }
    | otherwise   = return []
                       

pitch :: ABCParser ABCPitch
pitch = P.pitch

