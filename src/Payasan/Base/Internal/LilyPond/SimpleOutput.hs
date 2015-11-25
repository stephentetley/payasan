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

  , lilypondNotes

  ) where

import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJ        -- package: pretty


type Mon a = Rewrite State a

data State = State 
    { prev_info         :: !LocalContextInfo
    , opt_terminator    :: Maybe Doc 
    }

stateZero :: LocalContextInfo -> State
stateZero info = 
    State { prev_info  = info
          , opt_terminator = case local_meter info of 
                               Unmetered -> Just cadenzaOff_ 
                               _ -> Nothing 
          }


setInfo :: LocalContextInfo -> Mon () 
setInfo info = puts (\s -> s { prev_info = info })

getTerminator :: Mon (Maybe Doc)
getTerminator = gets opt_terminator

setTerminator :: Maybe Doc -> Mon ()
setTerminator optd = puts (\s -> s { opt_terminator = optd })


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
                     -> LyPhrase2 pch anno -> Doc
simpleScore_Relative def infos pch ph = 
        header 
    $+$ anonBlock (simpleVoice_Relative def pch ph)
  where
    header          = scoreHeader infos

simpleScore_Absolute :: LyOutputDef pch anno 
                     -> ScoreInfo 
                     -> LyPhrase2 pch anno -> Doc
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
                     -> LyPhrase2 pch anno -> Doc
simpleVoice_Relative def pch ph = 
    block (Just $ relative_ pch) (notes_header $+$ notes)
  where
    local1          = maybe default_local_info id $ firstContextInfo ph
    notes_header    = oPhraseHeader local1
    notes           = lilypondNotes def local1 ph


simpleVoice_Absolute :: LyOutputDef pch anno
                     -> LyPhrase2 pch anno -> Doc
simpleVoice_Absolute def ph = 
    absolute_ $+$ notes_header $+$ notes
  where
    local1          = maybe default_local_info id $ firstContextInfo ph
    notes_header    = oPhraseHeader local1
    notes           = lilypondNotes def local1 ph


oPhraseHeader :: LocalContextInfo -> Doc
oPhraseHeader locals = case local_meter locals of
    Unmetered -> cadenzaOn_ $+$ keyline
    TimeSig t -> keyline $+$ time_ t
  where
    keyline = key_  (local_key locals)


-- | Pitch should be \"context free\" at this point.
--
-- Design note - we only want to write this once.
-- Should allow different pch (standard, drum note, etc.)
-- to be printed. 
--
lilypondNotes :: forall pch anno. 
                 LyOutputDef pch anno 
              -> LocalContextInfo 
              -> LyPhrase2 pch anno 
              -> Doc
lilypondNotes def prefix_locals ph = 
    evalRewrite (final =<< oLyPhrase ph) (stateZero prefix_locals)
  where
    final d = do { od <- getTerminator
                 ; case od of Nothing -> return d
                              Just d1 -> return (d $+$ d1)
                 }

    pPitch :: pch -> Doc
    pPitch = printPitch def

    pAnno  :: anno -> Doc
    pAnno  = printAnno def

    oLyPhrase :: LyPhrase2 pch anno -> Mon Doc
    oLyPhrase (Phrase [])           = return empty
    oLyPhrase (Phrase (x:xs))       = do { d <- oBar x; step d xs }
      where
        step d []     = return d
        step d (b:bs) = do { d1    <- oBar b
                           ; let ac = d <+> char '|' $+$ d1
                           ; step ac bs 
                           }


    oBar :: LyBar2 pch anno -> Mon Doc
    oBar (Bar locals cs)            = 
          do { dkey     <- deltaKey locals
             ; dtime    <- deltaMetrical locals
             ; let ans  = hsep (map oNoteGroup cs)
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
                       TimeSig t -> setTerminator Nothing >> 
                                    return (time_ t $+$ d)
                   }



    oNoteGroup :: LyNoteGroup2 pch anno -> Doc
    oNoteGroup (Atom e)             = oElement e
    oNoteGroup (Beamed cs)          = beamForm $ map oNoteGroup cs
    oNoteGroup (Tuplet spec cs)     = tupletForm spec (map oNoteGroup cs)

    oElement :: LyElement2 pch anno -> Doc
    oElement (NoteElem n a t)       = oNote n <> pAnno a <> tie t

    oElement (Rest d)               = rest d 
    oElement (Spacer d)             = spacer d 
    oElement (Skip d)               = skip d 
    oElement (Chord ps d a t)       = 
        chordForm (map pPitch ps) <> noteLength d <> pAnno a <> tie t

    oElement (Graces ns)            = graceForm (map oNote ns)
    oElement (Punctuation s)        = text s


    oNote :: LyNote2 pch anno -> Doc
    oNote (Note p d)               = pPitch p <> noteLength d

