{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Chordmode.Internal.Parser
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Chordmode for LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.Chordmode.Internal.Parser
  ( 
     chordmode

  ) where

import Payasan.Chordmode.Internal.Base

import Payasan.Base.Internal.LilyPond.Lexer
import Payasan.Base.Internal.LilyPond.Parser
import Payasan.Base.Internal.LilyPond.Syntax

import Text.Parsec                              -- package: parsec

import Language.Haskell.TH.Quote



chordmode :: QuasiQuoter
chordmode = QuasiQuoter
    { quoteExp = \s -> case parseChordMode s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 



parseChordMode :: String -> Either ParseError (GenLyPhrase Pitch ChordSuffix)
parseChordMode = parseLyPhrase parsedef
  where
    parsedef = LyParserDef { pitchParser = chordRoot
                           , annoParser  = chordSuffix }


chordRoot :: LyParser Pitch
chordRoot = pitch

chordSuffix :: LyParser ChordSuffix
chordSuffix = (symbol ":" *> body) <|> pure NO_SUFFIX
  where
    body = ChordSuffix <$> chordModifier <*> steps

chordModifier :: LyParser ChordModifier
chordModifier = (try chordModifier1) <|> pure NO_MOD
             <?> "chordModifier"
 

chordModifier1 :: LyParser ChordModifier
chordModifier1 = choice $ 
    [ MAJ13     <$ symbol "maj13"
    , MAJ11     <$ symbol "maj11"
    , MAJ9      <$ symbol "maj9"
    , MAJ7      <$ symbol "maj7"
    , MAJ6      <$ symbol "6"
    , MAJ7      <$ symbol "maj"              -- alternative spelling for maj7
    , MM7       <$ symbol "m7+"              -- Mm7 - needs special case
    , MIN13     <$ symbol "m13"
    , MIN11     <$ symbol "m11"
    , MIN9      <$ symbol "m9"
    , MIN7      <$ symbol "m7"
    , MIN7      <$ symbol "m6"
    , MIN5      <$ symbol "m"
    , DIM7      <$ symbol "dim7"
    , DIM5      <$ symbol "dim"
    , AUG7      <$ symbol "aug7"
    , AUG5      <$ symbol "aug"
    , SUS4      <$ symbol "sus4"
    , SUS2      <$ symbol "sus2"
    , SUS       <$ symbol "sus"
    , DOM13     <$ symbol "13"
    , DOM11     <$ symbol "11"
    , DOM9      <$ symbol "9"
    , DOM7      <$ symbol "7"
    ] 
 
steps :: LyParser Steps
steps = (try body) <|> pure no_steps
     <?> "steps"
  where
    body        = Steps <$> steps1 <*> rest
    rest        = (try $ (symbol "^" *> steps1)) <|> pure []
    no_steps    = Steps [] []

steps1 :: LyParser [Step]
steps1 = sepBy step (symbol ".")

step :: LyParser Step
step = Step <$> int <*> alt

alt :: LyParser Alt
alt = choice [ pve, nve, pure NO_ALT ]
  where
    pve = PVE <$ symbol "+"
    nve = NVE <$ symbol "-"
