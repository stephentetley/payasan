{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.SimpleOutput
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.SimpleOutput
  ( 
    LyOutputDef(..)

  , simpleScore_Relative
  , simpleScore_Absolute

  , simpleVoice_Relative
  , simpleVoice_Absolute

  , scoreHeader
  , renderNotes

  ) where

import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJ        -- package: pretty


type Mon a = Rewrite State a

data State = State { prev_info  :: !LocalContextInfo }

stateZero :: LocalContextInfo -> State
stateZero info = State { prev_info  = info }


setInfo :: LocalContextInfo -> Mon () 
setInfo info = puts (\s -> s { prev_info = info })


deltaMetrical :: LocalContextInfo -> Mon (Maybe Meter)
deltaMetrical (LocalContextInfo { local_meter = m1 }) = 
    fn <$> gets prev_info
  where
    fn prev 
        | local_meter prev == m1 = Nothing
        | otherwise              = Just m1

deltaKey :: LocalContextInfo -> Mon (Maybe Key)
deltaKey (LocalContextInfo { local_key = k1 }) = 
    fn <$> gets prev_info
  where
    fn prev 
        | local_key prev == k1 = Nothing
        | otherwise            = Just k1


--------------------------------------------------------------------------------


data LyOutputDef pch anno = LyOutputDef 
    { printPitch    :: pch -> Doc
    , printAnno     :: anno -> Doc
    }



simpleScore_Relative :: LyOutputDef pch anno 
                     -> ScoreInfo 
                     -> Pitch
                     -> GenLyPhrase pch anno -> Doc
simpleScore_Relative def infos pch ph = 
        header 
    $+$ anonBlock (simpleVoice_Relative def pch ph)
  where
    header          = scoreHeader infos

simpleScore_Absolute :: LyOutputDef pch anno 
                     -> ScoreInfo 
                     -> GenLyPhrase pch anno -> Doc
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






--------------------------------------------------------------------------------
-- Notelist


-- @voiceOutput@ specifically for @standard@ pitch output.
--
-- Write alternative functions for other types of output.
-- 
simpleVoice_Relative :: LyOutputDef pch anno 
                     -> Pitch
                     -> GenLyPhrase pch anno -> Doc
simpleVoice_Relative def pch ph = 
    block (Just $ relative_ pch) (notes_header $+$ notes)
  where
    local1          = maybe default_local_info id $ firstContextInfo ph
    notes_header    = oPhraseHeader local1
    notes           = renderNotes def ph


simpleVoice_Absolute :: LyOutputDef pch anno
                     -> GenLyPhrase pch anno -> Doc
simpleVoice_Absolute def ph = 
    absolute_ $+$ notes_header $+$ notes
  where
    local1          = maybe default_local_info id $ firstContextInfo ph
    notes_header    = oPhraseHeader local1
    notes           = renderNotes def ph



oPhraseHeader :: LocalContextInfo -> Doc
oPhraseHeader locals = 
        key_  (local_key locals)
    $+$ time_ (local_meter locals)


-- | Pitch should be \"context free\" at this point.
--
-- Design note - we only want to write this once.
-- Should allow different pch (standard, drum note, etc.)
-- to be printed. 
--
renderNotes :: forall pch anno. 
               LyOutputDef pch anno -> GenLyPhrase pch anno -> Doc
renderNotes def ph = evalRewrite (oLyPhrase ph) (stateZero first_info)
  where
    first_info :: LocalContextInfo
    first_info  = maybe default_local_info id $ firstContextInfo ph

    pPitch :: pch -> Doc
    pPitch = printPitch def

    pAnno  :: anno -> Doc
    pAnno  = printAnno def

    oLyPhrase :: GenLyPhrase pch anno -> Mon Doc
    oLyPhrase (Phrase [])           = return empty
    oLyPhrase (Phrase (x:xs))       = do { d <- oBar x; step d xs }
      where
        step d []     = return d
        step d (b:bs) = do { d1    <- oBar b
                           ; let ac = d <+> char '|' $+$ d1
                           ; step ac bs 
                           }

    oBar :: GenLyBar pch anno -> Mon Doc
    oBar (Bar locals cs)            = 
          do { dkey     <- deltaKey locals
             ; dtime    <- deltaMetrical locals
             ; let ans  = hsep (map oNoteGroup cs)
             ; setInfo locals
             ; return $ prefixT dtime $ prefixK dkey $ ans
             }
        where
          prefixK Nothing   = (empty <>)
          prefixK (Just k)  = (key_ k $+$)
          prefixT Nothing   = (empty <>)
          prefixT (Just t)  = (time_ t $+$)

    oNoteGroup :: GenLyNoteGroup pch anno -> Doc
    oNoteGroup (Atom e)             = oElement e
    oNoteGroup (Beamed cs)          = beamForm $ map oNoteGroup cs
    oNoteGroup (Tuplet spec cs)     = tupletForm spec (map oNoteGroup cs)

    oElement :: GenLyElement pch anno -> Doc
    oElement (NoteElem n a t m)     = 
        oNote n <> pAnno a <> tie t <> renderMarkup m

    oElement (Rest d)               = rest d 
    oElement (Skip d)               = skip d 
    oElement (Chord ps d a t m)     = 
        chordForm (map pPitch ps) <> noteLength d <> pAnno a <> tie t <> renderMarkup m

    oElement (Graces ns)            = graceForm (map oNote ns)
    oElement (Punctuation s)        = text s


    oNote :: GenLyNote pch -> Doc
    oNote (Note p d)               = pPitch p <> noteLength d

