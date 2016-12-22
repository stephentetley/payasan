{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.LilyPond.SimpleOutput
-- Copyright   :  (c) Stephen Tetley 2015-2016
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
  , makeSimpleHeader

  , stateZero
  , simpleScore_Relative
  , simpleScore_Absolute

  , simpleVoice_Relative
  , simpleVoice_Absolute

  , scoreHeader
  , phraseHeader

  , makeLyNoteListDoc
  , lilypondNoteList

  ) where

import Payasan.PSC.LilyPond.Utils

import Payasan.PSC.Repr.External.Syntax

import Payasan.PSC.Base.LilyPondCommon
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.RewriteMonad
import Payasan.PSC.Base.Utils

import Payasan.Base.Basis
import Payasan.Base.Pitch
import Payasan.Base.Scale

import Text.PrettyPrint.HughesPJ        -- package: pretty


type Mon a = Rewrite () State a

-- Meter pattern (SectionInfo) is irrelevant at this stage.
-- Only care about Key and Meter

data State = State 
    { prev_key          :: !Key
    , prev_meter        :: !Meter
    }




stateZero :: SectionInfo -> State
stateZero info = 
    State { prev_key       = section_key info
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

makeSimpleHeader :: String -> String -> LyHeader
makeSimpleHeader vstring name = 
    TyDoc $ version_ vstring $+$ header
  where
    header  = withString name $ \ss ->
                 block (Just $ command "header") (title ss)



assembleSimpleLy :: LyHeader -> LyNoteListDoc -> Doc
assembleSimpleLy header body = extractDoc header $+$ extractDoc body
    

simpleScore_Relative :: LyOutputDef pch anno 
                     -> ScoreInfo 
                     -> Pitch
                     -> GenLyPartOut pch anno -> Doc
simpleScore_Relative def infos pch ph = 
        header 
    $+$ anonBlock (simpleVoice_Relative def pch ph)
  where
    header          = scoreHeader infos

simpleScore_Absolute :: LyOutputDef pch anno 
                     -> ScoreInfo 
                     -> GenLyPartOut pch anno -> Doc
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
                     -> GenLyPartOut pch anno -> Doc
simpleVoice_Relative def pch ph = 
    block (Just $ relative_ pch) (notes_header $+$ notes)
  where
    local1          = initialSectionInfo ph
    notes_header    = phraseHeader local1
    notes           = extractDoc $ lilypondNoteList def local1 ph


simpleVoice_Absolute :: LyOutputDef pch anno
                     -> GenLyPartOut pch anno -> Doc
simpleVoice_Absolute def ph = 
    absolute_ $+$ notes_header $+$ notes
  where
    local1          = initialSectionInfo ph
    notes_header    = phraseHeader local1
    notes           = extractDoc $ lilypondNoteList def local1 ph




fromRight :: Either z a -> a
fromRight (Right a) = a
fromRight _         = error "fromRight is really bad, to be removed soon."


-- TODO working towards an API that provides a (simple) "makeDoc"...
--
makeLyNoteListDoc :: forall pch anno. 
                     LyOutputDef pch anno 
                  -> SectionInfo 
                  -> GenLyPartOut pch anno
                  -> Mon LyNoteListDoc
makeLyNoteListDoc def info ph = TyDoc <$> oLyPart def ph


-- | Pitch should be \"context free\" at this point.
--
-- Design note - we only want to write this once.
-- Should allow different pch (standard, drum note, etc.)
-- to be printed. 
--
lilypondNoteList :: LyOutputDef pch anno 
                 -> SectionInfo 
                 -> GenLyPartOut pch anno
                 -> LyNoteListDoc
lilypondNoteList def prefix_locals ph = 
    fromRight $ evalRewrite (TyDoc <$> oLyPart def ph) () (stateZero prefix_locals)

 --   pPitch :: pch -> Doc
 --   pPitch = printPitch def

 --   pAnno  :: anno -> Doc
 --   pAnno  = printAnno def

oLyPart :: LyOutputDef pch anno -> Part pch LyNoteLength anno -> Mon Doc
oLyPart _   (Part [])           = return empty
oLyPart def (Part (x:xs))       = do { d <- oSection def x; step d xs }
  where
    step d []     = return d
    step d (s:ss) = do { d1    <- oSection def s
                       ; let ac = d <+> char '|' $+$ d1
                       ; step ac ss
                       }

-- Note - delta key implies standard pitch 
-- (i.e. not drum notes, neume names, etc...)
--
-- Should print section name first in a comment...
-- 
oSection :: LyOutputDef pch anno -> Section pch LyNoteLength anno -> Mon Doc
oSection def (Section _ locals bs) =
    do { dkeysig               <- deltaKeySig locals
       ; (dtime,dtimestop)     <- deltaTimeSig locals
       ; let ans = ppSection (bar_ "\\") $ map (oBar def) bs
       ; setInfo locals
       ; return (dtime ?+$ dkeysig ?+$ (ans $+? dtimestop))
       }


-- | Bars are not terminated...
oBar :: LyOutputDef pch anno -> Bar pch LyNoteLength anno -> Doc
oBar def (Bar cs) = hsep (map (oNoteGroup def) cs)


oNoteGroup :: LyOutputDef pch anno -> NoteGroup pch LyNoteLength anno -> Doc
oNoteGroup def grp              = fn grp
  where
    fn (Atom e)                 = oElement def e
    fn (Beamed cs)              = beamForm $ map fn cs
    fn (Tuplet spec cs)         = tupletForm spec (map fn cs)

oElement :: LyOutputDef pch anno -> Element pch LyNoteLength anno -> Doc
oElement def e                  = fn e
   where
     pPitch                     = printPitch def
     pAnno                      = printAnno def
     fn (NoteElem n a t)        = oNote def n <> pAnno a <> tie t
     fn (Rest d)                = rest d 
     fn (Spacer d)              = spacer d 
     fn (Skip d)                = skip d 
     fn (Chord ps d a t)        = 
        chordForm (map pPitch ps) <> noteLength d <> pAnno a <> tie t

     fn (Graces ns)            = graceForm $ map (oNote def) ns
     fn (Punctuation s)        = text s


oNote :: LyOutputDef pch anno -> Note pch LyNoteLength -> Doc
oNote def (Note p d)            = pPitch p <> noteLength d
   where
     pPitch = printPitch def



ppSection :: Doc -> [Doc] -> Doc
ppSection end bars = fn bars
  where
    fn []       = empty
    fn [b]      = b <+> end
    fn (b:bs)   = b <+> char '|' $+$ fn bs