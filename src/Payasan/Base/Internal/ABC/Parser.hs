{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.Parser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser for subset of ABC.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABC.Parser
  (
    abc

  -- * Elementary parsers
  , pitch
  , accidental
  , pitchLetter
  , noteLength
  , tupletSpec
  ) where

import Payasan.Base.Internal.ABC.Lexer
import Payasan.Base.Internal.ABC.Syntax
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax

import Text.Parsec                              -- package: parsec


import Language.Haskell.TH.Quote

import Data.Char (isSpace)

--------------------------------------------------------------------------------
-- Quasiquote

abc :: QuasiQuoter
abc = QuasiQuoter
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
phrase = Phrase <$> bars

bars :: ABCParser [ABCBar]
bars = sepBy bar barline

barline :: ABCParser ()
barline = reservedOp "|"

bar :: ABCParser ABCBar
bar = Bar default_local_info <$> noteGroups 


noteGroups :: ABCParser [ABCNoteGroup]
noteGroups = whiteSpace *> many noteGroup


noteGroup :: ABCParser ABCNoteGroup
noteGroup = tuplet <|> (Atom <$> element)

element :: ABCParser ABCElement
element = lexeme (rest <|> noteElem <|> chord <|> graces)

rest :: ABCParser ABCElement
rest = Rest <$> (char 'z' *> noteLength)

noteElem :: ABCParser ABCElement
noteElem = (\e -> NoteElem e ()) <$> note

chord :: ABCParser ABCElement
chord = (\ps d -> Chord ps d ())
          <$> squares (many1 pitch) <*> noteLength

graces :: ABCParser ABCElement
graces = Graces <$> braces (many1 note)


-- Cannot use parsecs count as ABC counts /deep leaves/.
--
tuplet :: ABCParser ABCNoteGroup
tuplet = do 
   spec   <- tupletSpec
   notes  <- countedNoteGroups (tuplet_len spec)
   return $ Tuplet spec notes

countedNoteGroups :: Int -> ABCParser [ABCNoteGroup]
countedNoteGroups n 
    | n > 0       = do { e  <- noteGroup
                       ; es <- countedNoteGroups (n - elementSize e)
                       ; return $ e:es
                       }
    | otherwise   = return []
                       
    

note :: ABCParser ABCNote
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
    octavehi = (OveRaised  . length) <$> many1 (char '\'')
    octavelo = (OveLowered . length) <$> many1 (char ',')



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


elementSize :: ABCNoteGroup -> Int
elementSize (Tuplet spec _) = tuplet_len spec
elementSize (Beamed xs)     = sum $ map elementSize xs
elementSize _               = 1

