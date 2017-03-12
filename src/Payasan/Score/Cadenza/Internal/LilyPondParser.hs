{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.LilyPondParser
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser for LilyPond'\s unmetered Cadenza mode. 
--
--------------------------------------------------------------------------------

module Payasan.Score.Cadenza.Internal.LilyPondParser
  (

    LyParserDef (..)    -- re-export
  , parseCadenzaNoAnno
  , parseCadenza

  , pitch               -- re-export
  , noAnno              -- re-export

  ) where


import Payasan.Score.Cadenza.Internal.Syntax

import Payasan.PSC.LilyPond.Lexer
import Payasan.PSC.LilyPond.Base ( LyPitch )
import qualified Payasan.PSC.LilyPond.ExternalParser    as P
import Payasan.PSC.LilyPond.ExternalParser (LyParserDef(..), pitch, noAnno)


import Text.Parsec                              -- package: parsec



--------------------------------------------------------------------------------
-- Parser


parseCadenzaNoAnno :: String -> Either ParseError (LySectionQuote LyPitch ())
parseCadenzaNoAnno = parseCadenza parsedef
  where
    parsedef = LyParserDef { pitchParser = pitch, annoParser = noAnno }


parseCadenza :: P.LyParserDef pch anno
              -> String 
              -> Either ParseError (LySectionQuote pch anno)
parseCadenza def = runParser (makeParser def) () ""




makeParser :: forall pch anno. 
              P.LyParserDef pch anno -> LyParser (LySectionQuote pch anno)
makeParser lib = fullParseLy section
  where
    pPitch :: LyParser pch
    pPitch = P.pitchParser lib

    pAnno  :: LyParser anno
    pAnno  = P.annoParser lib

    section :: LyParser (LySectionQuote pch anno)
    section = (LySectionQuote . reconcileBeamHeads) <$> noteGroups

    noteGroups :: LyParser [LyCadenzaNoteGroup2 pch anno]
    noteGroups = whiteSpace *> many noteGroup

    noteGroup :: LyParser (LyCadenzaNoteGroup2 pch anno)
    noteGroup = tuplet <|> beamTail <|> atom


    beamTail :: LyParser (LyCadenzaNoteGroup2 pch anno)
    beamTail = Beamed <$> squares noteGroups


    tuplet :: LyParser (LyCadenzaNoteGroup2 pch anno)
    tuplet = 
        (\spec notes -> Tuplet (P.makeTupletSpec spec (length notes)) notes)
            <$> P.tupletSpec <*> braces elements

    atom :: LyParser (LyCadenzaNoteGroup2 pch anno)
    atom = Atom <$> element

    elements :: LyParser [LyCadenzaElement2 pch anno]
    elements = whiteSpace *> many element

    element :: LyParser (LyCadenzaElement2 pch anno)
    element = lexeme (rest <|> note)

    note :: LyParser (LyCadenzaElement2 pch anno)
    note = (\p d a t -> Note p d a t) 
             <$> pPitch <*> P.noteLength <*> pAnno <*> P.tie
        <?> "note"

    rest :: LyParser (LyCadenzaElement2 pch anno)
    rest = Rest <$> (char 'r' *> P.noteLength)




-- | @reconcileBeamHeads@ expects sensible beam groups. 
-- It does not test for duration < quarter, or similar.
--
reconcileBeamHeads :: [LyCadenzaNoteGroup2 pch anno] 
                   -> [LyCadenzaNoteGroup2 pch anno]
reconcileBeamHeads = step1
  where
    step1 []               = []
    step1 (x:xs)           = step2 x xs
   
    step2 a (Beamed gs:bs) = Beamed (a:gs) : step1 bs
    step2 a (b:bs)         = a : step2 b bs
    step2 a []             = [a]



