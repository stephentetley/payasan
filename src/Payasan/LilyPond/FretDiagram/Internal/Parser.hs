{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Internal.Parser
-- Copyright   :  (c) Stephen Tetley 2015-2016
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

  ) where

import Payasan.LilyPond.FretDiagram.Internal.Base


import Payasan.Base.Internal.LilyPond.Lexer



import Text.Parsec                              -- package: parsec

import Language.Haskell.TH.Quote


-- Syntax is markup - not phrases.


fret_diagram :: QuasiQuoter
fret_diagram = QuasiQuoter
    { quoteExp = \s -> case parseFretDiagram s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 



parseFretDiagram :: String -> Either ParseError FretDiagram
parseFretDiagram = runParser (fullParseLy fretDiagram) () ""

fretDiagram :: LyParser FretDiagram
fretDiagram = (\a b -> FretDiagram { fd_name       = ""
                                   , fd_opt_barre  = a 
                                   , fd_fingerings = b }) 
          <$> (whiteSpace *> optionMaybe barre) <*> many1 fingering

fingering :: LyParser Fingering
fingering = p <?> "fingering"
  where
    p = Fingering <$> suffixStringNumber <*> (fretNumber <* symbol ";")


barre :: LyParser BarreIndicator
barre = BarreIndicator <$> (symbol "c:" *> suffixStringNumber) 
                       <*> suffixStringNumber
                       <*> (int <* symbol ";")




suffixStringNumber :: LyParser StringNumber
suffixStringNumber = stringNumber <* char '-'

stringNumber :: LyParser StringNumber
stringNumber = int

fretNumber :: LyParser FretNumber
fretNumber = choice [open, muted, number]
          <?> "fret-number"
  where
    open    = OPEN  <$ char 'o'
    muted   = MUTED <$ char 'x'
    number  = FretNumber <$> int

