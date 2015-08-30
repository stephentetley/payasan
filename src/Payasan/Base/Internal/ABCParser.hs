{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABCParser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Simple NoteList
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABCParser
  (
    abcPhrase

  -- * Elementary parsers
  , pitch
  , accidental
  , pitchLetter
  , noteLength
  , tupletSpec
  ) where

import Payasan.Base.Internal.ABCLexer
import Payasan.Base.Internal.ABCSyntax

import Text.Parsec                              -- package: parsec


import Language.Haskell.TH.Quote

import Data.Char (isSpace)

--------------------------------------------------------------------------------
-- Quasiquote

abcPhrase :: QuasiQuoter
abcPhrase = QuasiQuoter
    { quoteExp = \s -> case parseABCPhrase s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


--------------------------------------------------------------------------------
-- Parser


parseABCPhrase :: String -> Either ParseError ABCPhrase
parseABCPhrase = runParser fullABCPhrase () ""

fullABCPhrase :: ABCParser ABCPhrase
fullABCPhrase = whiteSpace *> abcPhraseK >>= step
  where 
    isTrail             = all (isSpace)
    step (ans,_,ss) 
        | isTrail ss    = return ans
        | otherwise     = fail $ "parseFail - remaining input: " ++ ss


abcPhraseK :: ABCParser (ABCPhrase,SourcePos,String)
abcPhraseK = (,,) <$> phrase <*> getPosition <*> getInput

phrase :: ABCParser ABCPhrase
phrase = ABCPhrase <$> bars

bars :: ABCParser [Bar]
bars = sepBy bar barline

barline :: ABCParser ()
barline = reservedOp "|"

bar :: ABCParser Bar
bar = Bar default_render_info <$> ctxElements 


ctxElements :: ABCParser [CtxElement]
ctxElements = whiteSpace *> many ctxElement


ctxElement :: ABCParser CtxElement
ctxElement = tuplet <|> (Atom <$> element)

element :: ABCParser Element
element = lexeme (rest <|> noteElem <|> chord <|> graces)

rest :: ABCParser Element
rest = Rest <$> (char 'z' *> noteLength)

noteElem :: ABCParser Element
noteElem = NoteElem <$> note

chord :: ABCParser Element
chord = Chord <$> squares (many1 pitch) <*> noteLength

graces :: ABCParser Element
graces = Graces <$> braces (many1 note)


-- Cannot use parsecs count as ABC counts /deep leaves/.
--
tuplet :: ABCParser CtxElement
tuplet = do 
   spec   <- tupletSpec
   notes  <- countedCtxElements (tuplet_len spec)
   return $ Tuplet spec notes

countedCtxElements :: Int -> ABCParser [CtxElement]
countedCtxElements n 
    | n > 0       = do { e  <- ctxElement
                       ; es <- countedCtxElements (n - elementSize e)
                       ; return $ e:es
                       }
    | otherwise   = return []
                       
    

note :: ABCParser Note
note = Note <$> pitch <*> noteLength
    <?> "note"


pitch :: ABCParser Pitch
pitch = Pitch <$> accidental <*> pitchLetter <*> octave





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


octave :: ABCParser Octave
octave = octavehi <|> octavelo <|> return OveDefault
  where
    octavehi = (OvePos . length) <$> many1 (char '\'')
    octavelo = (OveNeg . length) <$> many1 (char ',')



noteLength :: ABCParser NoteLength
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


--------------------------------------------------------------------------------
-- Helpers


elementSize :: CtxElement -> Int
elementSize (Tuplet spec _) = tuplet_len spec
elementSize (Beamed xs)     = sum $ map elementSize xs
elementSize _               = 1

