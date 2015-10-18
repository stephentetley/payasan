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

  , noAnno

  , makeTupletSpec

  ) where


import Payasan.Base.Internal.LilyPond.Lexer
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Duration

import Text.Parsec                              -- package: parsec


--------------------------------------------------------------------------------
-- Parser



data LyParserDef pch anno = LyParserDef 
    { pitchParser :: LyParser pch
    , annoParser  :: LyParser anno
    }


parseLyPhrase :: LyParserDef pch anno
              -> String 
              -> Either ParseError (GenLyPhrase pch anno)
parseLyPhrase def = runParser (makeLyParser def) () ""



makeLyParser :: forall pch anno. LyParserDef pch anno -> LyParser (GenLyPhrase pch anno)
makeLyParser def = fullInputParse phrase
  where
    pPitch :: LyParser pch
    pPitch = pitchParser def

    pAnno  :: LyParser anno
    pAnno  = annoParser def
    
    phrase :: LyParser (GenLyPhrase pch anno)
    phrase = Phrase <$> bars

    bars :: LyParser [GenLyBar pch anno]
    bars = sepBy bar barline

    bar :: LyParser (GenLyBar pch anno)
    bar = Bar default_local_info <$> noteGroups 

    noteGroups :: LyParser [GenLyNoteGroup pch anno]
    noteGroups = whiteSpace *> many noteGroup

    noteGroup :: LyParser (GenLyNoteGroup pch anno)
    noteGroup = tuplet <|> (Atom <$> element)

    -- | Unlike ABC, LilyPond does not need to count the number
    -- of notes in the tuplet to parse (they are properly enclosed 
    -- in braces).
    --
    tuplet :: LyParser (GenLyNoteGroup pch anno)
    tuplet = 
        (\spec notes -> Tuplet (makeTupletSpec spec (length notes)) notes)
            <$> tupletSpec <*> braces (noteGroups)


    element :: LyParser (GenLyElement pch anno)
    element = lexeme (rest <|> noteElem <|> chord <|> graces)


    noteElem :: LyParser (GenLyElement pch anno)
    noteElem = (\n a -> NoteElem n a NO_TIE no_markup) 
                  <$> note <*> pAnno

    rest :: LyParser (GenLyElement pch anno)
    rest = Rest <$> (char 'z' *> noteLength)

    chord :: LyParser (GenLyElement pch anno)
    chord = (\ps n a -> Chord ps n a NO_TIE no_markup)
                <$> angles (many1 pPitch) <*> noteLength <*> pAnno


    graces :: LyParser (GenLyElement pch anno)
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
    maxima  = d_maxima <$ reserved "\\maxima"
    longa   = d_longa  <$ reserved "\\longa"
    breve   = d_breve  <$ reserved "\\breve"
    

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

--------------------------------------------------------------------------------
-- Helpers


makeTupletSpec :: (Int,Int) -> Int -> TupletSpec
makeTupletSpec (n,t) len = 
    TupletSpec { tuplet_num         = n
               , tuplet_time_mult   = t
               , tuplet_len         = len
               }
