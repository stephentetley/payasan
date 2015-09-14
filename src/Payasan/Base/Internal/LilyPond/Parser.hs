{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.Parser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser for subset of LilyPond. Note - parser is parameteric
-- to handle alternative pitch notations.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.Parser
  (
    LyParserDef (..)
  , parseLyPhrase

  -- * Primitives
  , tupletSpec
  , barline

  , pitch
  , accidental
  , pitchLetter

  , noteLength

  , makeTupletSpec

  ) where


import Payasan.Base.Internal.LilyPond.Lexer
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Duration

import Text.Parsec                              -- package: parsec


import Data.Char (isSpace)


--------------------------------------------------------------------------------
-- Parser



data LyParserDef pch = LyParserDef 
    { pitchParser :: LyParser pch
    }


parseLyPhrase :: LyParserDef pch 
              -> String 
              -> Either ParseError (GenLyPhrase pch)
parseLyPhrase def = runParser (makeLyParser def) () ""



makeLyParser :: forall pch. LyParserDef pch -> LyParser (GenLyPhrase pch)
makeLyParser def = fullLyPhrase
  where
    pPitch :: LyParser pch
    pPitch = pitchParser def

    fullLyPhrase :: LyParser (GenLyPhrase pch)
    fullLyPhrase = whiteSpace *> lyPhraseK >>= step
      where 
        isTrail             = all (isSpace)
        step (ans,_,ss) 
            | isTrail ss    = return ans
            | otherwise     = fail $ "parseFail - remaining input: " ++ ss

    
    lyPhraseK :: LyParser (GenLyPhrase pch,SourcePos,String)
    lyPhraseK = (,,) <$> phrase <*> getPosition <*> getInput

    
    phrase :: LyParser (GenLyPhrase pch)
    phrase = Phrase <$> bars

    bars :: LyParser [GenLyBar pch]
    bars = sepBy bar barline


    bar :: LyParser (GenLyBar pch)
    bar = Bar default_local_info <$> ctxElements 

    ctxElements :: LyParser [GenLyCtxElement pch]
    ctxElements = whiteSpace *> many ctxElement

    ctxElement :: LyParser (GenLyCtxElement pch)
    ctxElement = tuplet <|> (Atom <$> element)

    -- | Unlike ABC, LilyPond does not need to count the number
    -- of notes in the tuplet to parse (they are properly enclosed 
    -- in braces).
    --
    tuplet :: LyParser (GenLyCtxElement pch)
    tuplet = 
        (\spec notes -> Tuplet (makeTupletSpec spec (length notes)) notes)
            <$> tupletSpec <*> braces (ctxElements)


    element :: LyParser (GenLyElement pch)
    element = lexeme (rest <|> noteElem <|> chord <|> graces)


    noteElem :: LyParser (GenLyElement pch)
    noteElem = NoteElem <$> note

    rest :: LyParser (GenLyElement pch)
    rest = Rest <$> (char 'z' *> noteLength)

    chord :: LyParser (GenLyElement pch)
    chord = Chord <$> angles (many1 pPitch) <*> noteLength


    graces :: LyParser (GenLyElement pch)
    graces = Graces <$> (reserved "\\grace" *> (multi <|> single))
      where
        multi   = braces (many1 note)
        single  = (\a -> [a]) <$> note


    note :: LyParser (GenLyNote pch)
    note = Note <$> pPitch <*> noteLength
        <?> "note"


tupletSpec :: LyParser (Int,Int)
tupletSpec = (,) <$> (reserved "\\tuplet" *> int) <*> (reservedOp "/" *> int)

barline :: LyParser ()
barline = reservedOp "|"


--------------------------------------------------------------------------------
-- Pitch Parser

-- | Middle c is c'
--
pitch :: LyParser Pitch
pitch = Pitch <$> pitchLetter <*> accidental <*> octaveModifier


pitchLetter :: LyParser PitchLetter
pitchLetter = choice $
    [ CL <$ char 'c'
    , DL <$ char 'd'
    , EL <$ char 'e'
    , FL <$ char 'f'
    , GL <$ char 'g'
    , AL <$ char 'a'
    , BL <$ char 'b'
    ]




octaveModifier :: LyParser Octave
octaveModifier = raised <|> lowered <|> dfault
  where
    raised  = OveRaised  <$> countOf (char '\'')
    lowered = OveLowered <$> countOf (char ',')
    dfault  = pure OveDefault

countOf :: LyParser a -> LyParser Int
countOf p = length <$> many1 p


-- | Sharps = @is@, flats = @es@.
--
accidental :: LyParser Accidental
accidental = accdntl <|> return NO_ACCIDENTAL
  where
    accdntl  = choice [ dblsharp, dblflat, sharp, flat ]
    dblsharp = DBL_SHARP <$ try (symbol "isis")
    dblflat  = DBL_FLAT  <$ try (symbol "eses")
    sharp    = SHARP     <$ symbol "is"
    flat     = FLAT      <$ symbol "es"









--------------------------------------------------------------------------------
-- Note length parser


noteLength :: LyParser NoteLength
noteLength = (try explicit) <|> dfault
  where
    dfault   = pure DrnDefault
    explicit = DrnExplicit <$> duration


duration :: LyParser Duration
duration = maxima <|> longa <|> breve <|> numeric
        <?> "duration"
  where
    maxima  = dMaxima <$ reserved "\\maxima"
    longa   = dLonga  <$ reserved "\\longa"
    breve   = dBreve  <$ reserved "\\breve"
    

numeric :: LyParser Duration
numeric = do { n <- int; ds <- many (char '.'); step n ds }
  where
    step 1   ds = return $ addDots (length ds) dWhole
    step 2   ds = return $ addDots (length ds) dHalf
    step 4   ds = return $ addDots (length ds) dQuarter
    step 8   ds = return $ addDots (length ds) dEighth
    step 16  ds = return $ addDots (length ds) dSixteenth
    step 32  ds = return $ addDots (length ds) dThirtySecondth
    step 64  ds = return $ addDots (length ds) dSixtyFourth
    step 128 ds = return $ addDots (length ds) dOneHundredAndTwentyEighth
    step n   _  = fail $ "Unrecognized duration length: " ++ show n




--------------------------------------------------------------------------------
-- Helpers


makeTupletSpec :: (Int,Int) -> Int -> TupletSpec
makeTupletSpec (n,t) len = 
    TupletSpec { tuplet_num   = n
               , tuplet_time  = t
               , tuplet_len   = len
               }
