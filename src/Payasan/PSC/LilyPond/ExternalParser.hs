{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.LilyPond.ExternalParser
-- Copyright   :  (c) Stephen Tetley 2015-2017
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

module Payasan.PSC.LilyPond.ExternalParser
  (

  -- * Parsers
    
    LyParserDef (..)

  , parseLilyPondNoAnno
  , parseLySectionQuote
  , makeLyParser
  
  
  -- * Primitives
  , tupletSpec
  , barline
  , command

  , pitch
  , accidental
  , pitchLetter

  , noteLength

  , noAnno
  , tie

  , makeTupletSpec

  ) where

import Payasan.PSC.LilyPond.Base
import Payasan.PSC.LilyPond.Lexer

import Payasan.PSC.Repr.External.Syntax

import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration

import Text.Parsec                              -- package: parsec




--------------------------------------------------------------------------------
-- Parser


data LyParserDef pch anno = LyParserDef 
    { pitchParser :: LyParser pch
    , annoParser  :: LyParser anno
    }


parseLilyPondNoAnno :: String -> Either ParseError (LySectionQuote LyPitch ())
parseLilyPondNoAnno = parseLySectionQuote parsedef
  where
    parsedef = LyParserDef { pitchParser = pitch, annoParser = noAnno }
    
    

parseLySectionQuote :: LyParserDef pch anno
                    -> String 
                    -> Either ParseError (LySectionQuote pch anno)
parseLySectionQuote def = runParser (makeLyParser def) () ""



-- | This parser reads beam groups literally even if they do 
-- not make metrical sense.
--
-- Note that some pipelines may lose original beaming and 
-- try to resynthesize it.
--
makeLyParser :: forall pch anno. LyParserDef pch anno -> LyParser (LySectionQuote pch anno)
makeLyParser def = fullParseLy qsection
  where
    pPitch :: LyParser pch
    pPitch = pitchParser def

    pAnno  :: LyParser anno
    pAnno  = annoParser def
    
    qsection :: LyParser (LySectionQuote pch anno)
    qsection = (\bs -> LySectionQuote { getLySectionQuote = bs }) <$> bars

    bars :: LyParser [Bar pch LyNoteLength anno]
    bars = sepBy bar barline

    -- Beaming is not strictly nested in LilyPond (i.e. beam 
    -- start comes after the first member b[eam]), 
    -- so we do a post-processing step to reconcile the first 
    -- member of the beam group.
    --
    bar :: LyParser (Bar pch LyNoteLength anno)
    bar = Bar <$> noteGroups 

    noteGroups :: LyParser [NoteGroup pch LyNoteLength anno]
    noteGroups = concat <$> (whiteSpace *> many noteGroup)

    noteGroup :: LyParser [NoteGroup pch LyNoteLength anno]
    noteGroup = mult tuplet <|> beamTail <|> mult atom
      where
        mult p = (\a -> [a]) <$> p


    beamTail :: LyParser [NoteGroup pch LyNoteLength anno]
    beamTail = squares noteGroups


    -- Unlike ABC, LilyPond does not need to count the number
    -- of notes in the tuplet to parse (they are properly enclosed 
    -- in braces).
    --
    tuplet :: LyParser (NoteGroup pch LyNoteLength anno)
    tuplet = 
        (\spec notes -> Tuplet (makeTupletSpec spec (length notes)) notes)
            <$> tupletSpec <*> braces (noteGroups)


    atom :: LyParser (NoteGroup pch LyNoteLength anno)
    atom = Atom <$> element

    element :: LyParser (Element pch LyNoteLength anno)
    element = lexeme (rest <|> note <|> chord <|> graces)


    note :: LyParser (Element pch LyNoteLength anno)
    note = Note <$> pPitch <*> noteLength <*> pAnno <*> tie

    rest :: LyParser (Element pch LyNoteLength anno)
    rest = Rest <$> (char 'r' *> noteLength)

    chord :: LyParser (Element pch LyNoteLength anno)
    chord = (\ps n a t -> Chord ps n a t)
                <$> angles (many1 pPitch) <*> noteLength <*> pAnno <*> tie

    graces :: LyParser (Element pch LyNoteLength anno)
    graces = Graces <$> (command "grace" *> (multi <|> single))
      where
        multi   = braces (many1 grace1)
        single  = (\a -> [a]) <$> grace1


    grace1 :: LyParser (Grace1 pch LyNoteLength)
    grace1 = Grace1 <$> pPitch <*> noteLength
        <?> "grace1"


tupletSpec :: LyParser (Int,Int)
tupletSpec = (,) <$> (command "tuplet" *> int) <*> (reservedOp "/" *> int)

barline :: LyParser ()
barline = reservedOp "|"



command :: String -> LyParser String
command s = try $ symbol ('\\' : s)


--------------------------------------------------------------------------------
-- Pitch Parser

-- | Middle c is c'
--
pitch :: LyParser LyPitch
pitch = LyPitch <$> pitchLetter <*> accidental <*> octaveModifier


pitchLetter :: LyParser PitchLetter
pitchLetter = choice $
    [ C <$ char 'c'
    , D <$ char 'd'
    , E <$ char 'e'
    , F <$ char 'f'
    , G <$ char 'g'
    , A <$ char 'a'
    , B <$ char 'b'
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


noteLength :: LyParser LyNoteLength
noteLength = (try explicit) <|> dfault
  where
    dfault   = pure DrnDefault
    explicit = DrnExplicit <$> duration


duration :: LyParser Duration
duration = maxima <|> longa <|> breve <|> numeric
        <?> "duration"
  where
    maxima  = d_maxima <$ command "maxima"
    longa   = d_longa  <$ command "longa"
    breve   = d_breve  <$ command "breve"
    

numeric :: LyParser Duration
numeric = do { n <- int; ds <- many (char '.'); step n ds }
  where
    step 1   ds = return $ addDots (length ds) d_whole
    step 2   ds = return $ addDots (length ds) d_half
    step 4   ds = return $ addDots (length ds) d_quarter
    step 8   ds = return $ addDots (length ds) d_eighth
    step 16  ds = return $ addDots (length ds) d_sixteenth
    step 32  ds = return $ addDots (length ds) d_thirty_secondth
    step 64  ds = return $ addDots (length ds) d_sixty_fourth
    step 128 ds = return $ addDots (length ds) d_one_hundred_and_twenty_eighth
    step n   _  = fail $ "Unrecognized duration length: " ++ show n




noAnno :: LyParser ()
noAnno = return ()


tie :: LyParser Tie
tie = atie <|> notie
  where
    atie  = TIE <$ try (whiteSpace >> symbol "~")
    notie = return NO_TIE 


--------------------------------------------------------------------------------
-- Helpers


makeTupletSpec :: (Int,Int) -> Int -> TupletSpec
makeTupletSpec (n,t) len = 
    TupletSpec { tuplet_num         = n
               , tuplet_time_mult   = t
               , tuplet_len         = len
               }
