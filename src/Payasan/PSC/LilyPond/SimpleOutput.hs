{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.LilyPond.SimpleOutput
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.PSC.LilyPond.SimpleOutput
  ( 
    LyOutputDef(..)

  , LyHeader
  , makeHeader

  , assembleLy

  , stateZero
  , simpleScore_Relative
  , simpleScore_Absolute

  , simpleVoice_Relative
  , simpleVoice_Absolute

  , scoreHeader
  , phraseHeader

  , makeLyNoteList
  , lilypondNoteList

  ) where

import Payasan.PSC.LilyPond.Common
import Payasan.PSC.LilyPond.Utils

import Payasan.PSC.Repr.External.Syntax

import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils

import Payasan.Base.Basis
import Payasan.Base.Pitch
import Payasan.Base.Scale

import Text.PrettyPrint.HughesPJ        -- package: pretty

import Control.Monad.State


type Mon a = State St a

-- Meter pattern (SectionInfo) is irrelevant at this stage.
-- Only care about Key and Meter

data St = St 
    { prev_key          :: !Key
    , prev_meter        :: !Meter
    }




stateZero :: SectionInfo -> St
stateZero info = 
    St { prev_key       = section_key info
       , prev_meter     = section_meter info
       }


setInfo :: SectionInfo -> Mon () 
setInfo info = modify (\s -> s { prev_key = section_key info
                               , prev_meter = section_meter info })


-- | Always returns Unmetered even if prev was Unmetered
deltaMetrical :: SectionInfo -> Mon (Maybe Meter)
deltaMetrical (SectionInfo { section_meter = m1 }) = 
    fn <$> gets prev_meter
  where
    fn prev 
        | prev == m1 && m1 /= Unmetered   = Nothing
        | otherwise                       = Just m1



deltaKey :: SectionInfo -> Mon (Maybe Key)
deltaKey (SectionInfo { section_key = k1 }) = 
    fn <$> gets prev_key
  where
    fn prev 
        | prev == k1    = Nothing
        | otherwise     = Just k1


deltaKeySig :: SectionInfo -> Mon (Maybe Doc)
deltaKeySig info = fmap key_ <$> deltaKey info


-- Should only coalesce proper time signatures not cadenzas

deltaTimeSig :: SectionInfo -> Mon (Maybe Doc, Maybe Doc)
deltaTimeSig info = fn <$> deltaMetrical info
  where
    fn Nothing             = (Nothing, Nothing)
    fn (Just (Unmetered))  = (Just cadenzaOn_, Just cadenzaOff_)
    fn (Just (Metered t))  = (Just $ time_ t, Nothing)

--------------------------------------------------------------------------------


data LyOutputDef pch anno = LyOutputDef 
    { printPitch    :: pch -> Doc
    , printAnno     :: anno -> Doc
    }

data LyHeader_
type LyHeader = TyDoc LyHeader_

makeHeader :: String -> String -> LyHeader
makeHeader vstring name = 
    TyDoc $ version_ vstring $+$ header
  where
    header  = withString name $ \ss ->
                 block (Just $ command "header") (title ss)



assembleLy :: LyHeader -> LyNoteList -> Doc
assembleLy header body = extractDoc header $+$ extractDoc body
    

simpleScore_Relative :: LyOutputDef pch anno 
                     -> ScoreInfo 
                     -> Pitch
                     -> Part pch LyNoteLength anno 
                     -> Doc
simpleScore_Relative def infos pch ph = 
        header 
    $+$ anonBlock (simpleVoice_Relative def pch ph)
  where
    header          = scoreHeader infos

simpleScore_Absolute :: LyOutputDef pch anno 
                     -> ScoreInfo 
                     -> Part pch LyNoteLength anno 
                     -> Doc
simpleScore_Absolute def infos ph = 
        header 
    $+$ anonBlock (simpleVoice_Absolute def ph)
  where
    header          = scoreHeader infos


scoreHeader :: ScoreInfo -> Doc
scoreHeader globals = 
    version_ (score_ly_version globals) $+$ header
  where
    header  = withString (score_title globals) $ \ss ->
                 block (Just $ command "header") (title ss)



phraseHeader :: SectionInfo -> Doc
phraseHeader locals = case section_meter locals of
    Unmetered -> cadenzaOn_ $+$ keyline
    Metered t -> keyline $+$ time_ t
  where
    keyline = key_ (section_key locals)



--------------------------------------------------------------------------------
-- Notelist


-- @voiceOutput@ specifically for @standard@ pitch output.
--
-- Write alternative functions for other types of output.
-- 
simpleVoice_Relative :: LyOutputDef pch anno 
                     -> Pitch
                     -> Part pch LyNoteLength anno -> Doc
simpleVoice_Relative def pch ph = 
    block (Just $ relative_ pch) (notes_header $+$ notes)
  where
    local1          = initialSectionInfo ph
    notes_header    = phraseHeader local1
    notes           = extractDoc $ lilypondNoteList def local1 ph


simpleVoice_Absolute :: LyOutputDef pch anno
                     -> Part pch LyNoteLength anno -> Doc
simpleVoice_Absolute def ph = 
    absolute_ $+$ notes_header $+$ notes
  where
    local1          = initialSectionInfo ph
    notes_header    = phraseHeader local1
    notes           = extractDoc $ lilypondNoteList def local1 ph






-- TODO working towards an API that provides a (simple) "makeDoc"...
--
makeLyNoteList :: LyOutputDef pch anno 
               -> SectionInfo
               -> Part pch LyNoteLength anno
               -> LyNoteList
makeLyNoteList def info ph =
    TyDoc $ evalState (renderLyPartM def ph) (stateZero info)


-- | Pitch should be \"context free\" at this point.
--
-- Design note - we only want to write this once.
-- Should allow different pch (standard, drum note, etc.)
-- to be printed. 
--
lilypondNoteList :: LyOutputDef pch anno 
                 -> SectionInfo 
                 -> Part pch LyNoteLength anno
                 -> LyNoteList
lilypondNoteList def prefix_locals ph = 
    TyDoc $ evalState (renderLyPartM def ph) (stateZero prefix_locals)



renderLyPartM :: LyOutputDef pch anno 
              -> Part pch LyNoteLength anno 
              -> Mon Doc
renderLyPartM lib (Part { part_sections = xs }) = 
    vsep <$> mapM (renderSectionM lib) xs


-- Note - delta key implies standard pitch 
-- (i.e. not drum notes, neume names, etc...)
--
-- Should print section name first in a comment...
-- 
renderSectionM :: LyOutputDef pch anno 
               -> Section pch LyNoteLength anno 
               -> Mon Doc
renderSectionM lib (Section { section_info = locals
                            , section_bars = bs }) =
    do { dkeysig               <- deltaKeySig locals
       ; (dtime,dtimestop)     <- deltaTimeSig locals
       ; let ans = ppSection (bar_ "\\") $ map (renderBar lib) bs
       ; setInfo locals
       ; return (dtime ?+$ dkeysig ?+$ (ans $+? dtimestop))
       }


-- | Bars are not terminated...
--
renderBar :: LyOutputDef pch anno -> Bar pch LyNoteLength anno -> Doc
renderBar lib (Bar { note_groups = xs }) = hsep (map noteGroup xs)
  where
    pPitch                          = printPitch lib
    pAnno                           = printAnno lib

    -- Decons NoteGroup
    noteGroup (Atom e)              = element e
    noteGroup (Beamed cs)           = beamForm $ map noteGroup cs
    noteGroup (Tuplet spec cs)      = tupletForm spec (map noteGroup cs)

    -- Decons Element
    element (Note p d a t)          = pPitch p <> noteLength d <> pAnno a <> tie t
    element (Rest d)                = rest d 
    element (Spacer d)              = spacer d 
    element (Skip d)                = skip d 
    element (Chord ps d a t)        = 
        chordForm (map pPitch ps) <> noteLength d <> pAnno a <> tie t

    element (Graces ns)            = graceForm $ map grace1 ns
    element (Punctuation s)        = text s
     
    -- Decons Grace1
    grace1 (Grace1 p d)            = pPitch p <> noteLength d


ppSection :: Doc -> [Doc] -> Doc
ppSection end bars = fn bars
  where
    fn []       = empty
    fn [b]      = b <+> end
    fn (b:bs)   = b <+> char '|' $+$ fn bs