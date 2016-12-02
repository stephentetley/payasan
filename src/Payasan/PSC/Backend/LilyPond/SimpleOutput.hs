{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Backend.LilyPond.SimpleOutput
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

module Payasan.PSC.Backend.LilyPond.SimpleOutput
  ( 
    LyOutputDef(..)

  , simpleScore_Relative
  , simpleScore_Absolute

  , simpleVoice_Relative
  , simpleVoice_Absolute

  , scoreHeader
  , phraseHeader

  , lilypondNoteList

  ) where

import Payasan.PSC.Backend.LilyPond.Utils

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

data State = State 
    { prev_info         :: !SectionInfo
    , opt_terminator    :: Maybe Doc 
    }

stateZero :: SectionInfo -> State
stateZero info = 
    State { prev_info  = info
          , opt_terminator = case section_meter info of 
                               Unmetered -> Just cadenzaOff_ 
                               _ -> Nothing 
          }


setInfo :: SectionInfo -> Mon () 
setInfo info = puts (\s -> s { prev_info = info })

getTerminator :: Mon (Maybe Doc)
getTerminator = gets opt_terminator

setTerminator :: Maybe Doc -> Mon ()
setTerminator optd = puts (\s -> s { opt_terminator = optd })


deltaMetrical :: SectionInfo -> Mon (Maybe Meter)
deltaMetrical (SectionInfo { section_meter = m1 }) = 
    fn <$> gets prev_info
  where
    fn prev 
        | section_meter prev == m1 = Nothing
        | otherwise             = Just m1

deltaKey :: SectionInfo -> Mon (Maybe Key)
deltaKey (SectionInfo { section_key = k1 }) = 
    fn <$> gets prev_info
  where
    fn prev 
        | section_key prev == k1 = Nothing
        | otherwise           = Just k1


--------------------------------------------------------------------------------


data LyOutputDef pch anno = LyOutputDef 
    { printPitch    :: pch -> Doc
    , printAnno     :: anno -> Doc
    }



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
    notes           = getLyNoteListDoc $ lilypondNoteList def local1 ph


simpleVoice_Absolute :: LyOutputDef pch anno
                     -> GenLyPartOut pch anno -> Doc
simpleVoice_Absolute def ph = 
    absolute_ $+$ notes_header $+$ notes
  where
    local1          = initialSectionInfo ph
    notes_header    = phraseHeader local1
    notes           = getLyNoteListDoc $ lilypondNoteList def local1 ph




fromRight :: Either z a -> a
fromRight (Right a) = a
fromRight _         = error "fromRight is really bad, to be removed soon."


-- | Pitch should be \"context free\" at this point.
--
-- Design note - we only want to write this once.
-- Should allow different pch (standard, drum note, etc.)
-- to be printed. 
--
lilypondNoteList :: forall pch anno. 
                    LyOutputDef pch anno 
                 -> SectionInfo 
                 -> GenLyPartOut pch anno
                 -> LyNoteListDoc
lilypondNoteList def prefix_locals ph = 
    fromRight $ evalRewrite (final =<< oLyPart ph) () (stateZero prefix_locals)
  where
    final d = do { od <- getTerminator
                 ; case od of Nothing -> return $ LyNoteListDoc d
                              Just d1 -> return $ LyNoteListDoc (d $+$ d1)
                 }

    pPitch :: pch -> Doc
    pPitch = printPitch def

    pAnno  :: anno -> Doc
    pAnno  = printAnno def

    oLyPart :: Part pch LyNoteLength anno -> Mon Doc
    oLyPart (Part [])               = return empty
    oLyPart (Part (x:xs))           = do { d <- oSection x; step d xs }
      where
        step d []     = return d
        step d (s:ss) = do { d1    <- oSection s
                           ; let ac = d <+> char '|' $+$ d1
                           ; step ac ss
                           }

    oSection :: Section pch LyNoteLength anno -> Mon Doc
    oSection (Section _ locals bs) =
          do { dkey     <- deltaKey locals
             ; dtime    <- deltaMetrical locals
             ; let ans = vsep $ map oBar bs
             ; setInfo locals
             ; mwrapT dtime $ prefixK dkey $ ans
             }
        where
          prefixK (Nothing) d   = d
          prefixK (Just k)  d   = key_ k $+$ d
          mwrapT (Nothing)  d   = return d
          mwrapT (Just m)   d   = 
                do { d0 <- getTerminator
                   ; case m of  
                       Unmetered -> setTerminator (Just cadenzaOff_) >>
                                    return (d0 $?+$ cadenzaOn_ $+$ d)
                       Metered t -> setTerminator Nothing >> 
                                    return (time_ t $+$ d)
                   }


    -- | Bars are terminated...
    oBar :: Bar pch LyNoteLength anno -> Doc
    oBar (Bar cs) = hsep (map oNoteGroup cs) <+> char '|'


    oNoteGroup :: NoteGroup pch LyNoteLength anno -> Doc
    oNoteGroup (Atom e)             = oElement e
    oNoteGroup (Beamed cs)          = beamForm $ map oNoteGroup cs
    oNoteGroup (Tuplet spec cs)     = tupletForm spec (map oNoteGroup cs)

    oElement :: Element pch LyNoteLength anno -> Doc
    oElement (NoteElem n a t)       = oNote n <> pAnno a <> tie t

    oElement (Rest d)               = rest d 
    oElement (Spacer d)             = spacer d 
    oElement (Skip d)               = skip d 
    oElement (Chord ps d a t)       = 
        chordForm (map pPitch ps) <> noteLength d <> pAnno a <> tie t

    oElement (Graces ns)            = graceForm (map oNote ns)
    oElement (Punctuation s)        = text s


    oNote :: Note pch LyNoteLength -> Doc
    oNote (Note p d)               = pPitch p <> noteLength d

