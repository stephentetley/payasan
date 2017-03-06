{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.ABC.ExternalParser
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser for subset of ABC.
--
--------------------------------------------------------------------------------

module Payasan.PSC.ABC.ExternalParser
  (

    parseABCPart


  -- * Elementary parsers
  , pitch
  , accidental
  , pitchLetter
  , noteLength
  , tupletSpec
  , tie

  ) where


import Payasan.PSC.ABC.Base
import Payasan.PSC.ABC.Lexer

import Payasan.PSC.Repr.External.Syntax

import Payasan.PSC.Base.SyntaxCommon

import Text.Parsec                              -- package: parsec



--------------------------------------------------------------------------------
-- Parser

parseABCPart :: String -> Either ParseError ABCSectionQuote
parseABCPart = runParser (fullParseABC qsection) () ""


qsection :: ABCParser ABCSectionQuote
qsection = (\bs -> ABCSectionQuote { getABCSection = bs}) <$> bars

bars :: ABCParser [Bar ABCPitch ABCNoteLength ()]
bars = sepBy bar barline

barline :: ABCParser ()
barline = reservedOp "|"

bar :: ABCParser (Bar ABCPitch ABCNoteLength ())
bar = Bar <$> noteGroups 

noteGroups :: ABCParser [NoteGroup ABCPitch ABCNoteLength ()]
noteGroups = whiteSpace *> many noteGroup

noteGroup :: ABCParser (NoteGroup ABCPitch ABCNoteLength ())
noteGroup = tuplet <|> (Atom <$> element)

element :: ABCParser (Element ABCPitch ABCNoteLength ())
element = lexeme (rest <|> note <|> chord <|> graces)

rest :: ABCParser (Element ABCPitch ABCNoteLength ())
rest = Rest <$> (char 'z' *> noteLength)

note :: ABCParser (Element ABCPitch ABCNoteLength ())
note = (\p d t -> Note p d () t) <$> pitch <*> noteLength <*> tie

chord :: ABCParser (Element ABCPitch ABCNoteLength ())
chord = (\ps d t -> Chord ps d () t)
          <$> squares (many1 pitch) <*> noteLength <*> tie

graces :: ABCParser (Element ABCPitch ABCNoteLength ())
graces = Graces <$> braces (many1 grace1)


-- Cannot use parsecs count as ABC counts /deep leaves/.
--
tuplet :: ABCParser (NoteGroup ABCPitch ABCNoteLength ())
tuplet = do 
   spec   <- tupletSpec
   notes  <- countedNoteGroups (tuplet_len spec)
   return $ Tuplet spec notes

countedNoteGroups :: Int -> ABCParser [NoteGroup ABCPitch ABCNoteLength ()]
countedNoteGroups n 
    | n > 0       = do { e  <- noteGroup
                       ; es <- countedNoteGroups (n - elementSize e)
                       ; return $ e:es
                       }
    | otherwise   = return []
                       
    

grace1 :: ABCParser (Grace1 ABCPitch ABCNoteLength)
grace1 = Grace1 <$> pitch <*> noteLength
    <?> "grace1"


pitch :: ABCParser ABCPitch
pitch = ABCPitch <$> accidental <*> pitchLetter <*> octaveModifier





accidental :: ABCParser Accidental
accidental = accdntl <|> return NO_ACCIDENTAL
  where
    accdntl = choice $ 
                [ DBL_FLAT  <$ try (symbol "__")
                , FLAT      <$ symbol "_"
                , DBL_SHARP <$ try (symbol "^^")
                , SHARP     <$ symbol "^"
                , NATURAL   <$ symbol "="
                ]


pitchLetter :: ABCParser PitchLetter
pitchLetter = choice $
    [ CU <$ char 'C'
    , DU <$ char 'D'
    , EU <$ char 'E'
    , FU <$ char 'F'
    , GU <$ char 'G'
    , AU <$ char 'A'
    , BU <$ char 'B'
    , CL <$ char 'c'
    , DL <$ char 'd'
    , EL <$ char 'e'
    , FL <$ char 'f'
    , GL <$ char 'g'
    , AL <$ char 'a'
    , BL <$ char 'b'
    ]


octaveModifier :: ABCParser OctaveModifier
octaveModifier = octavehi <|> octavelo <|> return OveDefault
  where
    octavehi = (OveRaised  . length) <$> many1 (char '\'')
    octavelo = (OveLowered . length) <$> many1 (char ',')



noteLength :: ABCParser ABCNoteLength
noteLength = try (divd <|> multOrFrac) 
          <|> return DNL
  where
    divd        = Divd <$> divider
    multOrFrac  = do { n <- int 
                     ; opt <- optionMaybe (symbol "/" *> int)
                     ; case opt of
                         Nothing -> return $ Mult n
                         Just d  -> return $ Frac n d
                     }

divider :: ABCParser Int
divider = symbol "/" *> (int <|> moreDivs 2) 
       <?> "divider"
  where
    moreDivs ac = symbol "/" *> moreDivs (ac*2)
               <|> return ac
    

tupletSpec :: ABCParser TupletSpec
tupletSpec = symbol "(" *> int >>= step1
  where
    step1 n   =  do { t <- (symbol ":" *> int); step2 n t }
             <|> decode1 n
    step2 n t = do { x <- (symbol ":" *> int); return $ TupletSpec n t x }
             <|> decode2 n t

    decode1 2 = return $ TupletSpec 2 3 2
    decode1 3 = return $ TupletSpec 3 2 3
    decode1 4 = return $ TupletSpec 4 3 4
    decode1 6 = return $ TupletSpec 6 2 6
    decode1 8 = return $ TupletSpec 8 3 8
    decode1 n = fail $ "invalid tuplet spec (" ++ show n

    decode2 n t = return $ TupletSpec n t n


tie :: ABCParser Tie
tie = atie <|> notie
  where
    atie  = TIE <$ try (whiteSpace >> symbol "-")
    notie = return NO_TIE 

--------------------------------------------------------------------------------
-- Helpers


elementSize :: NoteGroup ABCPitch ABCNoteLength () -> Int
elementSize (Tuplet spec _) = tuplet_len spec
elementSize _               = 1

