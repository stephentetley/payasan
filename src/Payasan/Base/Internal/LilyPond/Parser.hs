{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.Parser
-- Copyright   :  (c) Stephen Tetley 2015-2016
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

    lilypond
  , LyParserDef (..)
  , parseLyPart
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


import Payasan.Base.Internal.LilyPond.Lexer
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Repr.IRBeam.Syntax
import Payasan.Base.Internal.SyntaxCommon
import Payasan.Base.Duration

import Text.Parsec                              -- package: parsec

import Language.Haskell.TH.Quote                -- package: template-haskell



--------------------------------------------------------------------------------
-- Quasiquote

lilypond :: QuasiQuoter
lilypond = QuasiQuoter
    { quoteExp = \s -> case parseLilyPondNoAnno s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 

--------------------------------------------------------------------------------
-- Parser


data LyParserDef pch anno = LyParserDef 
    { pitchParser :: LyParser pch
    , annoParser  :: LyParser anno
    }


parseLilyPondNoAnno :: String -> Either ParseError (LyPart1 ())
parseLilyPondNoAnno = parseLyPart parsedef
  where
    parsedef = LyParserDef { pitchParser = pitch, annoParser = noAnno }


parseLyPart :: LyParserDef pch anno
            -> String 
            -> Either ParseError (LyPart2 pch anno)
parseLyPart def = runParser (makeLyParser def) () ""



--
-- | This parser reads beam groups literally even though they are 
-- lost in the translation to Main syntax.
--
-- [Beaming is re-synthesized for final output].
--
makeLyParser :: forall pch anno. LyParserDef pch anno -> LyParser (LyPart2 pch anno)
makeLyParser def = fullParseLy part
  where
    pPitch :: LyParser pch
    pPitch = pitchParser def

    pAnno  :: LyParser anno
    pAnno  = annoParser def
    
    part :: LyParser (LyPart2 pch anno)
    part = Part <$> bars

    bars :: LyParser [LyBar2 pch anno]
    bars = sepBy bar barline

    -- Beaming is not strictly nested in LilyPond (e.g. b[eam]), 
    -- so we do a post-processing step to reconcile the first 
    -- member of the beam group.
    --
    bar :: LyParser (LyBar2 pch anno)
    bar = (Bar default_section_info . reconcileBeamHeads) <$> noteGroups 

    noteGroups :: LyParser [LyNoteGroup2 pch anno]
    noteGroups = whiteSpace *> many noteGroup

    noteGroup :: LyParser (LyNoteGroup2 pch anno)
    noteGroup = tuplet <|> beamTail <|> atom


    beamTail :: LyParser (LyNoteGroup2 pch anno)
    beamTail = Beamed <$> squares noteGroups


    -- Unlike ABC, LilyPond does not need to count the number
    -- of notes in the tuplet to parse (they are properly enclosed 
    -- in braces).
    --
    tuplet :: LyParser (LyNoteGroup2 pch anno)
    tuplet = 
        (\spec notes -> Tuplet (makeTupletSpec spec (length notes)) notes)
            <$> tupletSpec <*> braces (noteGroups)



    atom :: LyParser (LyNoteGroup2 pch anno)
    atom = Atom <$> element

    element :: LyParser (LyElement2 pch anno)
    element = lexeme (rest <|> noteElem <|> chord <|> graces)


    noteElem :: LyParser (LyElement2 pch anno)
    noteElem = (\n a t -> NoteElem n a t) 
                  <$> note <*> pAnno <*> tie

    rest :: LyParser (LyElement2 pch anno)
    rest = Rest <$> (char 'r' *> noteLength)

    chord :: LyParser (LyElement2 pch anno)
    chord = (\ps n a t -> Chord ps n a t)
                <$> angles (many1 pPitch) <*> noteLength <*> pAnno <*> tie


    graces :: LyParser (LyElement2 pch anno)
    graces = Graces <$> (command "grace" *> (multi <|> single))
      where
        multi   = braces (many1 note)
        single  = (\a -> [a]) <$> note


    note :: LyParser (LyNote2 pch anno)
    note = Note <$> pPitch <*> noteLength
        <?> "note"


tupletSpec :: LyParser (Int,Int)
tupletSpec = (,) <$> (command "tuplet" *> int) <*> (reservedOp "/" *> int)

barline :: LyParser ()
barline = reservedOp "|"



command :: String -> LyParser String
command s = try $ symbol ('\\' : s)


-- | @reconcileBeamHeads@ expects sensible beam groups. 
-- It does not test for duration < quarter, or similar.
--
reconcileBeamHeads :: [LyNoteGroup2 pch anno] -> [LyNoteGroup2 pch anno]
reconcileBeamHeads = step1
  where
    step1 []               = []
    step1 (x:xs)           = step2 x xs
   
    step2 a (Beamed gs:bs) = Beamed (a:gs) : step1 bs
    step2 a (b:bs)         = a : step2 b bs
    step2 a []             = [a]


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
