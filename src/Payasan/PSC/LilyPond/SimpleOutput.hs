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
  , makeLyHeader

  , assembleLy

  , simpleScore_Relative
  , simpleScore_Absolute

  , simpleVoice_Relative
  , simpleVoice_Absolute

  , scoreHeader
  , phraseHeader

  , makeLyNoteList
  , lilypondNoteList

  ) where

import Payasan.PSC.LilyPond.Base
import Payasan.PSC.LilyPond.Pretty

import Payasan.PSC.Repr.External.Syntax
import qualified Payasan.PSC.LilyPond.IRSimpleDoc as DOC

import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils

import Payasan.Base.Basis
import Payasan.Base.Pitch
import Payasan.Base.Scale

import Text.PrettyPrint.HughesPJ        -- package: pretty




data LyOutputDef pch anno = LyOutputDef 
    { printPitch    :: pch -> Doc
    , printAnno     :: anno -> Doc
    }

data LyHeader_
type LyHeader = TyDoc LyHeader_



makeLyHeader :: String -> String -> LyHeader
makeLyHeader vstring name = 
    TyDoc $ version_ vstring $+$ header
  where
    header  = withString name $ \ss ->
                 block (Just $ command "header") (title ss)


-- Probably wrong - separate notions of header and notelist
-- is looking mistaken...
--
assembleLy :: LyHeader -> LyNoteList -> Doc
assembleLy header body = extractDoc header $+$ extractDoc body
    

simpleScore_Relative :: LyOutputDef pch anno 
                     -> String
                     -> String
                     -> Pitch
                     -> Part pch LyNoteLength anno 
                     -> Doc
simpleScore_Relative def lyversion name pch ph = 
        header 
    $+$ anonBlock (simpleVoice_Relative def pch ph)
  where
    header          = scoreHeader lyversion name

simpleScore_Absolute :: LyOutputDef pch anno 
                     -> String 
                     -> String
                     -> Part pch LyNoteLength anno 
                     -> Doc
simpleScore_Absolute def lyversion name ph = 
        header 
    $+$ anonBlock (simpleVoice_Absolute def ph)
  where
    header          = scoreHeader lyversion name


scoreHeader :: String -> String -> Doc
scoreHeader lyversion name  = 
    version_ lyversion $+$ header
  where
    header  = withString name $ \ss ->
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
    notes           = extractDoc $ makeLyNoteList def ph


simpleVoice_Absolute :: LyOutputDef pch anno
                     -> Part pch LyNoteLength anno -> Doc
simpleVoice_Absolute def ph = 
    absolute_ $+$ notes_header $+$ notes
  where
    local1          = initialSectionInfo ph
    notes_header    = phraseHeader local1
    notes           = extractDoc $ makeLyNoteList def ph


-- NOTE 
-- Delta changes might not be the best approach for LilyPond.
-- Might be better to have each section print its context, even
-- if there are no changes.
--


meterCtx :: Meter -> DOC.ContextDoc
meterCtx (Unmetered) = 
    DOC.ContextDoc $ \d -> cadenzaOn_ $+$ d $+$ cadenzaOff_
    
meterCtx (Metered t) = 
    DOC.ContextDoc $ \d -> time_ t $+$ d

keyCtx :: Key -> DOC.ContextDoc
keyCtx k = DOC.ContextDoc $ \d -> key_ k $+$ d
    

relativeCtx :: Pitch -> DOC.ContextDoc
relativeCtx p = DOC.ContextDoc $ \d -> block (Just $ relative_ p) d

absoluteCtx :: DOC.ContextDoc
absoluteCtx = DOC.ContextDoc $ \d -> absolute_ $+$ d


makeLyNoteList :: LyOutputDef pch anno 
               -> Part pch LyNoteLength anno
               -> LyNoteList
makeLyNoteList lib part =
    TyDoc $ renderDocPart $ toIRSimpleDoc lib part

lilypondNoteList :: LyOutputDef pch anno 
                 -> SectionInfo 
                 -> Part pch LyNoteLength anno
                 -> LyNoteList
lilypondNoteList lib _ part = makeLyNoteList lib part




toIRSimpleDoc :: LyOutputDef pch anno -> Part pch LyNoteLength anno -> DOC.Part a
toIRSimpleDoc lib (Part { part_sections = ss}) = 
    DOC.Part { DOC.part_sections = map sectionT ss }
  where
    sectionT (Section { section_name = name
                      , section_info = info
                      , section_bars = bars }) = 
            DOC.Section { DOC.section_name    = name
                        , DOC.section_context = makeContext info
                        , DOC.section_bars    = map barT bars }
    
    barT bar = DOC.Bar { DOC.bar_content = renderBar lib bar }


makeContext :: SectionInfo -> DOC.ContextDoc
makeContext (SectionInfo { section_meter = m1
                         , section_key   = k1 }) = 
    meterCtx m1 `mappend` keyCtx k1


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


renderDocPart :: DOC.Part a -> Doc
renderDocPart (DOC.Part { DOC.part_sections = ss }) = sectionsD ss
  where
    sectionsD []     = empty
    sectionsD [d]    = sectionD (text "|]") d
    sectionsD (d:ds) = sectionD (text "|")  d $+$ sectionsD ds

    sectionD end (DOC.Section { DOC.section_context = ctx
                              , DOC.section_bars    = dbars }) = 
          let dbars1 = map DOC.bar_content dbars 
              body   = ppSection end dbars1
          in DOC.getContext ctx $ body


