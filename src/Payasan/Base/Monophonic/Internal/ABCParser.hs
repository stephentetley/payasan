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

import Payasan.Base.Monophonic.Internal.Syntax

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
phrase = Phrase default_section_info <$> bars

bars :: ABCParser [ABCMonoBar]
bars = sepBy bar barline

barline :: ABCParser ()
barline = reservedOp "|"

bar :: ABCParser ABCMonoBar
bar = Bar <$> noteGroups 


noteGroups :: ABCParser [ABCMonoNoteGroup]
noteGroups = whiteSpace *> many noteGroup


noteGroup :: ABCParser ABCMonoNoteGroup
noteGroup = tuplet <|> (Atom <$> element)

element :: ABCParser ABCMonoElement
element = lexeme (rest <|> note)

rest :: ABCParser ABCMonoElement
rest = Rest <$> (char 'z' *> P.noteLength)

note :: ABCParser ABCMonoElement
note = (\p d t -> Note p d () t) <$> pitch <*> P.noteLength <*> P.tie
    <?> "note"


-- Cannot use parsecs count as ABC counts /deep leaves/.
--
tuplet :: ABCParser ABCMonoNoteGroup
tuplet = do 
   spec   <- P.tupletSpec
   notes  <- countedElements (tuplet_len spec)
   return $ Tuplet spec notes

countedElements :: Int -> ABCParser [ABCMonoElement]
countedElements n 
    | n > 0       = do { e  <- element
                       ; es <- countedElements (n - 1)
                       ; return $ e:es
                       }
    | otherwise   = return []
                       

pitch :: ABCParser ABCPitch
pitch = P.pitch

