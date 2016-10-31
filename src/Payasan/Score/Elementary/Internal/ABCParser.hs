{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.ABCParser
-- Copyright   :  (c) Stephen Tetley 2015-2016
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
    abc
  ) where

import Payasan.Score.Elementary.Internal.Syntax

import Payasan.PSC.Base.ABCLexer
import qualified Payasan.PSC.Repr.External.ABCParser as P
import Payasan.PSC.Base.ABCCommon

import Payasan.PSC.Base.SyntaxCommon


import Text.Parsec                              -- package: parsec


import Language.Haskell.TH.Quote

import Data.Char (isSpace)

--------------------------------------------------------------------------------
-- Quasiquote

-- Note - unfortunately we can\'t parameterize the quasiquoter
-- (e.g. with default note length)


abc :: QuasiQuoter
abc = QuasiQuoter
    { quoteExp = \s -> case parseABCPart s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


--------------------------------------------------------------------------------
-- Parser



parseABCPart :: String -> Either ParseError ABCElemPart
parseABCPart = runParser fullABCPart () ""




fullABCPart :: ABCParser ABCElemPart
fullABCPart = whiteSpace *> abcPartK >>= step
  where 
    isTrail             = all (isSpace)
    step (ans,_,ss) 
        | isTrail ss    = return ans
        | otherwise     = fail $ "parseFail - remaining input: " ++ ss


abcPartK :: ABCParser (ABCElemPart,SourcePos,String)
abcPartK = (,,) <$> part <*> getPosition <*> getInput

part :: ABCParser ABCElemPart 
part = Part default_section_info <$> bars

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

