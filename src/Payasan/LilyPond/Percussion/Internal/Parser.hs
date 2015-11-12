{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Percussion.Internal.Parser
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser for LilyPond drum mode.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Percussion.Internal.Parser
  (
    drums
  , makeDrumsParser

  ) where

import Payasan.LilyPond.Percussion.Internal.Base

import Payasan.Base.Internal.LilyPond.Lexer
import Payasan.Base.Internal.LilyPond.Parser
import Payasan.Base.Internal.LilyPond.Syntax

import Text.Parsec                              -- package: parsec


import Language.Haskell.TH.Quote



--------------------------------------------------------------------------------
-- Quasiquote

-- Design note - use the lilypond mode command name @\drums\ as the 
-- quasiquoter name.

drums :: QuasiQuoter
drums = QuasiQuoter
    { quoteExp = \s -> case parseLyDrums s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


--------------------------------------------------------------------------------
-- Parser


parseLyDrums :: String -> Either ParseError (LyPhrase2 DrumPitch Accent)
parseLyDrums = parseLyPhrase (makeDrumsDef accent)


makeDrumsParser :: LyParser anno -> LyParser (LyPhrase2 DrumPitch anno)
makeDrumsParser pAnno = makeLyParser (makeDrumsDef pAnno)


makeDrumsDef :: LyParser anno -> LyParserDef DrumPitch anno
makeDrumsDef pAnno = LyParserDef { pitchParser = drumPitch, annoParser = pAnno }

accent :: LyParser Accent
accent = try acc1 <|> return NO_ACCENT
  where
    acc1 = ACCENT <$ symbol "->"

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


