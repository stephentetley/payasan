{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Cadenza.Internal.Parser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser for LilyPond'\s unmetered Cadenza mode. 
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Cadenza.Internal.Parser
  (

    cadenza
  , LyParserDef (..)    -- re-export
  , parseCadenza
  , pitch               -- re-export
  , noAnno              -- re-export

  ) where


import Payasan.LilyPond.Cadenza.Internal.Syntax

import Payasan.Base.Internal.LilyPond.Lexer
import qualified Payasan.Base.Internal.LilyPond.Parser as P
import Payasan.Base.Internal.LilyPond.Parser (LyParserDef(..), pitch, noAnno)
import Payasan.Base.Internal.CommonSyntax


import Text.Parsec                              -- package: parsec

import Language.Haskell.TH.Quote                -- package: template-haskell




--------------------------------------------------------------------------------
-- Quasiquote

cadenza :: QuasiQuoter
cadenza = QuasiQuoter
    { quoteExp = \s -> case parseCadenzaNoAnno s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


--------------------------------------------------------------------------------
-- Parser


parseCadenzaNoAnno :: String -> Either ParseError (LyCadenzaPhrase1 ())
parseCadenzaNoAnno = parseCadenza parsedef
  where
    parsedef = LyParserDef { pitchParser = pitch, annoParser = noAnno }


parseCadenza :: P.LyParserDef pch anno
              -> String 
              -> Either ParseError (LyCadenzaPhrase2 pch anno)
parseCadenza def = runParser (makeParser def) () ""



-- TODO - handle beaming... 

makeParser :: forall pch anno. 
              P.LyParserDef pch anno -> LyParser (LyCadenzaPhrase2 pch anno)
makeParser def = fullParseLy phrase
  where
    pPitch :: LyParser pch
    pPitch = P.pitchParser def

    pAnno  :: LyParser anno
    pAnno  = P.annoParser def

    phrase :: LyParser (LyCadenzaPhrase2 pch anno)
    phrase = (Phrase default_local_info . reconcileBeamHeads) <$> noteGroups

    noteGroups :: LyParser [LyCadenzaNoteGroup2 pch anno]
    noteGroups = whiteSpace *> many noteGroup

    noteGroup :: LyParser (LyCadenzaNoteGroup2 pch anno)
    noteGroup = tuplet <|> beamTail <|> atom


    beamTail :: LyParser (LyCadenzaNoteGroup2 pch anno)
    beamTail = Beamed <$> squares noteGroups


    tuplet :: LyParser (LyCadenzaNoteGroup2 pch anno)
    tuplet = 
        (\spec notes -> Tuplet (P.makeTupletSpec spec (length notes)) notes)
            <$> P.tupletSpec <*> braces (noteGroups)

    atom :: LyParser (LyCadenzaNoteGroup2 pch anno)
    atom = Atom <$> element

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


