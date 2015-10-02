{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Internal.Parser
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Fret diagrams parser.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.FretDiagram.Internal.Parser
  ( 
     fret_diagram

  , testIt

  ) where

import Payasan.LilyPond.FretDiagram.Internal.Base


import Payasan.Base.Internal.LilyPond.Lexer



import Text.Parsec                              -- package: parsec

import Language.Haskell.TH.Quote

-- Syntax is markup - not phrases.



fret_diagram :: QuasiQuoter
fret_diagram = QuasiQuoter
    { quoteExp = \s -> case parseFretting s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 



parseFretting :: String -> Either ParseError Fretting
parseFretting = runParser (fullInputParse fretting) () ""

fretting :: LyParser Fretting
fretting = whiteSpace *> many1 (fretting1 <* symbol ";")

fretting1 :: LyParser Fretting1
fretting1 = barre <|> fret
         <?> "fretting1"
  where
    barre = Barre <$> (symbol "c:" *> many1 (try $ stringNumber <* char '-'))
                  <*> fretNumber
    fret  = Fret <$> (stringNumber <* char '-') <*> fretNumber




stringNumber :: LyParser StringNumber
stringNumber = int

fretNumber :: LyParser FretNumber
fretNumber = choice [open, muted, number]
          <?> "fret-number"
  where
    open    = OPEN  <$ char 'o'
    muted   = MUTED <$ char 'x'
    number  = FretNumber <$> int

testIt = runParser fretting () "" "6-x;5-x;4-o;3-2;2-3;1-2; "