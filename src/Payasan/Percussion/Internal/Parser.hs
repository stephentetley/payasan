{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Percussion.Internal.Parser
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

module Payasan.Percussion.Internal.Parser
  (
    drums

  -- * Elementary parsers
  , note
  , rest
  , tupletSpec
  , drumPitch
  , noteLength
  , transTupletSpec
  ) where

import Payasan.Percussion.Internal.Base

import Payasan.Base.Internal.LilyPond.Lexer
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Duration

import Text.Parsec                              -- package: parsec


import Language.Haskell.TH.Quote

import Data.Char (isSpace)


--------------------------------------------------------------------------------
-- Quasiquote

-- Design note - use the lilypond mode command name @\drums\ as the 
-- quasiquoter name.

drums :: QuasiQuoter
drums = QuasiQuoter
    { quoteExp = \s -> case parseLyPhrase s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


--------------------------------------------------------------------------------
-- Parser





parseLyPhrase :: String -> Either ParseError (GenLyPhrase DrumPitch)
parseLyPhrase = runParser fullLyPhrase () ""

fullLyPhrase :: LyParser (GenLyPhrase DrumPitch)
fullLyPhrase = whiteSpace *> lyPhraseK >>= step
  where 
    isTrail             = all (isSpace)
    step (ans,_,ss) 
        | isTrail ss    = return ans
        | otherwise     = fail $ "parseFail - remaining input: " ++ ss


lyPhraseK :: LyParser (GenLyPhrase DrumPitch,SourcePos,String)
lyPhraseK = (,,) <$> phrase <*> getPosition <*> getInput


phrase :: LyParser (GenLyPhrase DrumPitch)
phrase = Phrase <$> bars

bars :: LyParser [GenLyBar DrumPitch]
bars = sepBy bar barline

barline :: LyParser ()
barline = reservedOp "|"

bar :: LyParser (GenLyBar DrumPitch)
bar = Bar default_local_info <$> ctxElements 


ctxElements :: LyParser [GenLyCtxElement DrumPitch]
ctxElements = whiteSpace *> many ctxElement

ctxElement :: LyParser (GenLyCtxElement DrumPitch)
ctxElement = tuplet <|> (Atom <$> element)


element :: LyParser (GenLyElement DrumPitch)
element = lexeme (rest <|> noteElem <|> chord <|> graces)



noteElem :: LyParser (GenLyElement DrumPitch)
noteElem = NoteElem <$> note

rest :: LyParser (GenLyElement DrumPitch)
rest = Rest <$> (char 'z' *> noteLength)

chord :: LyParser (GenLyElement DrumPitch)
chord = Chord <$> angles (many1 drumPitch) <*> noteLength


graces :: LyParser (GenLyElement DrumPitch)
graces = Graces <$> (reserved "\\grace" *> (multi <|> single))
  where
    multi   = braces (many1 note)
    single  = (\a -> [a]) <$> note


tuplet :: LyParser (GenLyCtxElement DrumPitch)
tuplet = 
    (\spec notes -> Tuplet (transTupletSpec spec (length notes)) notes)
      <$> tupletSpec <*> braces (ctxElements)


tupletSpec :: LyParser LyTupletSpec
tupletSpec = LyTupletSpec <$> (reserved "\\tuplet" *> int)
                          <*> (reservedOp "/" *> int)



note :: LyParser (GenLyNote DrumPitch)
note = Note <$> drumPitch <*> noteLength
    <?> "note"


drumPitch :: LyParser DrumPitch
drumPitch = choice $ map try $
    [ Hiagogo           <$ symbol "agh"
    , Loagogo           <$ symbol "agl"
    , Acousticbassdrum  <$ symbol "bda"
    , Bassdrum          <$ symbol "bd"
    , Mutehibongo       <$ symbol "bohm"
    , Hibongo           <$ symbol "boh"
    , Openhibongo       <$ symbol "boho"
    , Mutelobongo       <$ symbol "bolm"
    , Openlobongo       <$ symbol "bolo"
    , Lobongo           <$ symbol "bol"
    , Cabasa            <$ symbol "cab"
    , Cowbell           <$ symbol "cb"
    , Mutehiconga       <$ symbol "cghm"
    , Openhiconga       <$ symbol "cgho"
    , Hiconga           <$ symbol "cgh"
    , Muteloconga       <$ symbol "cglm"
    , Openloconga       <$ symbol "cglo"
    , Loconga           <$ symbol "cgl"
    , Claves            <$ symbol "cl" 
    , Mutecuica         <$ symbol "cuim" 
    , Opencuica         <$ symbol "cuio" 
    , Crashcymbala      <$ symbol "cymca"
    , Crashcymbalb      <$ symbol "cymcb"
    , Chinesecymbal     <$ symbol "cymch"
    , Crashcymbal       <$ symbol "cymc"
    , Ridecymbala       <$ symbol "cymra"
    , Ridecymbalb       <$ symbol "cymrb"
    , Ridecymbal        <$ symbol "cymr"
    , Splashcymbal      <$ symbol "cyms"
    , Onedown           <$ symbol "da" 
    , Twodown           <$ symbol "db" 
    , Threedown         <$ symbol "dc" 
    , Fourdown          <$ symbol "dd" 
    , Fivedown          <$ symbol "de" 
    , Longguiro         <$ symbol "guil" 
    , Shortguiro        <$ symbol "guis" 
    , Guiro             <$ symbol "gui"
    , Handclap          <$ symbol "hc" 
    , Closedhihat       <$ symbol "hhc" 
    , Halfopenhihat     <$ symbol "hhho"
    , Openhihat         <$ symbol "hho"
    , Pedalhihat        <$ symbol "hhp"
    , Hihat             <$ symbol "hh"
    , Maracas           <$ symbol "mar"
    , Ridebell          <$ symbol "rb"
    , Acousticsnare     <$ symbol "sna"
    , Electricsnare     <$ symbol "sne"
    , Snare             <$ symbol "sn"
    , Hisidestick       <$ symbol "ssh" 
    , Losidestick       <$ symbol "ssl"
    , Sidestick         <$ symbol "ss"
    , Tambourine        <$ symbol "tamb"
    , Mutetriangle      <$ symbol "trim" 
    , Opentriangle      <$ symbol "trio" 
    , Triangle          <$ symbol "tri" 
    , Hitimbale         <$ symbol "timh"
    , Lotimbale         <$ symbol "timl"
    , Highfloortom      <$ symbol "tomfh"
    , Lowfloortom       <$ symbol "tomfl"
    , Hightom           <$ symbol "tomh"
    , Lowtom            <$ symbol "toml"
    , Himidtom          <$ symbol "tommh"
    , Lowmidtom         <$ symbol "tomml"
    , Vibraslap         <$ symbol "vibs"
    , Oneup             <$ symbol "ua" 
    , Twoup             <$ symbol "ub" 
    , Threeup           <$ symbol "uc" 
    , Fourup            <$ symbol "ud"
    , Fiveup            <$ symbol "ue" 
    , Hiwoodblock       <$ symbol "wbh" 
    , Lowoodblock       <$ symbol "wbl" 
    , Longwhistle       <$ symbol "whl"
    , Shortwhistle      <$ symbol "whs"
    ]


-- | Note - LilyPond has more drum pitches than MIDI and
-- there does not always seem to be a correspondence when 
-- they match.

-- counting1 :: LyParser a -> LyParser Int
-- counting1 p = length <$> many1 p


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

transTupletSpec :: LyTupletSpec -> Int -> TupletSpec 
transTupletSpec (LyTupletSpec n t) len = 
    TupletSpec { tuplet_num   = n
               , tuplet_time  = t
               , tuplet_len   = len
               }
