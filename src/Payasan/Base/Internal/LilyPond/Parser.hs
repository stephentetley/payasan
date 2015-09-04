{-# LANGUAGE TemplateHaskell            #-}
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
-- Parser for subset of LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.Parser
  (
    lilypond

  -- * Elementary parsers
  , note
  , rest
  , tupletSpec
  , pitch
  , accidental
  , pitchLetter
  , noteLength
  ) where


import Payasan.Base.Internal.LilyPond.Lexer
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Duration

import Text.Parsec                              -- package: parsec


import Language.Haskell.TH.Quote

import Data.Char (isSpace)


--------------------------------------------------------------------------------
-- Quasiquote

lilypond :: QuasiQuoter
lilypond = QuasiQuoter
    { quoteExp = \s -> case parseLyPhrase s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


--------------------------------------------------------------------------------
-- Parser





parseLyPhrase :: String -> Either ParseError LyPhrase
parseLyPhrase = runParser fullLyPhrase () ""

fullLyPhrase :: LilyPondParser LyPhrase
fullLyPhrase = whiteSpace *> lyPhraseK >>= step
  where 
    isTrail             = all (isSpace)
    step (ans,_,ss) 
        | isTrail ss    = return ans
        | otherwise     = fail $ "parseFail - remaining input: " ++ ss


lyPhraseK :: LilyPondParser (LyPhrase,SourcePos,String)
lyPhraseK = (,,) <$> phrase <*> getPosition <*> getInput


phrase :: LilyPondParser LyPhrase
phrase = Phrase <$> bars

bars :: LilyPondParser [LyBar]
bars = sepBy bar barline

barline :: LilyPondParser ()
barline = reservedOp "|"

bar :: LilyPondParser LyBar
bar = Bar default_local_info <$> ctxElements 


ctxElements :: LilyPondParser [LyCtxElement]
ctxElements = whiteSpace *> many ctxElement

ctxElement :: LilyPondParser LyCtxElement
ctxElement = tuplet <|> (Atom <$> element)


element :: LilyPondParser LyElement
element = lexeme (rest <|> noteElem <|> chord <|> graces)



noteElem :: LilyPondParser LyElement
noteElem = NoteElem <$> note

rest :: LilyPondParser LyElement
rest = Rest <$> (char 'z' *> noteLength)

chord :: LilyPondParser LyElement
chord = Chord <$> angles (many1 pitch) <*> noteLength


graces :: LilyPondParser LyElement
graces = Graces <$> (reserved "\\grace" *> (multi <|> single))
  where
    multi   = braces (many1 note)
    single  = (\a -> [a]) <$> note


tuplet :: LilyPondParser LyCtxElement
tuplet = 
    (\spec notes -> Tuplet (transTupletSpec spec (length notes)) notes)
      <$> tupletSpec <*> braces (ctxElements)

tupletSpec :: LilyPondParser LyTupletSpec
tupletSpec = LyTupletSpec <$> (reserved "\\tuplet" *> int)
                          <*> (reservedOp "/" *> int)



note :: LilyPondParser LyNote
note = Note <$> pitch <*> noteLength
    <?> "note"



-- | Middle c is c'
--
pitch :: LilyPondParser Pitch
pitch = Pitch <$> pitchLetter <*> accidental <*> octaveModifier


pitchLetter :: LilyPondParser PitchLetter
pitchLetter = choice $
    [ CL <$ char 'c'
    , DL <$ char 'd'
    , EL <$ char 'e'
    , FL <$ char 'f'
    , GL <$ char 'g'
    , AL <$ char 'a'
    , BL <$ char 'b'
    ]




octaveModifier :: LilyPondParser Octave
octaveModifier = raised <|> lowered <|> dfault
  where
    raised  = OveRaised  <$> counting1 (char '\'')
    lowered = OveLowered <$> counting1 (char ',')
    dfault  = pure OveDefault

accidental :: LilyPondParser Accidental
accidental = accdntl <|> return NO_ACCIDENTAL
  where
    accdntl  = choice [ dblsharp, dblflat, sharp, flat ]
    dblsharp = DBL_SHARP <$ try (symbol "isis")
    dblflat  = DBL_FLAT  <$ try (symbol "eses")
    sharp    = SHARP     <$ symbol "es"
    flat     = FLAT      <$ symbol "is"

counting1 :: LilyPondParser a -> LilyPondParser Int
counting1 p = length <$> many1 p


noteLength :: LilyPondParser NoteLength
noteLength = (try explicit) <|> dfault
  where
    dfault   = pure DrnDefault
    explicit = DrnExplicit <$> duration


duration :: LilyPondParser Duration
duration = maxima <|> longa <|> breve <|> numeric
        <?> "duration"
  where
    maxima  = dMaxima <$ reserved "\\maxima"
    longa   = dLonga  <$ reserved "\\longa"
    breve   = dBreve  <$ reserved "\\breve"
    

numeric :: LilyPondParser Duration
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

transTupletSpec :: LyTupletSpec -> Int -> TupletSpec 
transTupletSpec (LyTupletSpec n t) len = 
    TupletSpec { tuplet_num   = n
               , tuplet_time  = t
               , tuplet_len   = len
               }
