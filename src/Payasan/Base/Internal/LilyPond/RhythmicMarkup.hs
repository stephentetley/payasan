{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.RhythmicMarkup
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- For debugging - print /pitch content/ as markup. Typeset
-- notes on a rhythmic staff.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.RhythmicMarkup
  (
    MarkupOutput(..)
  , translateToRhythmicMarkup

  , rhythmicMarkupScore
  , rhythmicMarkupVoice

  ) where



import Payasan.Base.Internal.LilyPond.OutTrans
import Payasan.Base.Internal.LilyPond.SimpleOutput
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals
import Payasan.Base.Internal.CommonSyntax

import Payasan.Base.Duration


import Text.PrettyPrint.HughesPJ        -- package: pretty



data MarkupOutput pch = MarkupOutput { asMarkup :: pch -> Markup }


translateToRhythmicMarkup :: MarkupOutput pch
                          -> Phrase pch Duration anno 
                          -> Phrase LyPitch LyNoteLength anno
translateToRhythmicMarkup mo = 
    transformP (markup_algo mo) . translateToOutput_DurationOnly




--------------------------------------------------------------------------------
-- Pitch to markup translation

markup_algo :: MarkupOutput pch -> BeamPitchAlgo () pch LyPitch
markup_algo mo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = liftElementTrafo $ elementP mo
    }


elementP :: forall pch drn anno. 
            MarkupOutput pch 
         -> Element pch drn anno 
         -> Element LyPitch drn anno
elementP mo elt = case elt of 
    NoteElem e a t _    -> NoteElem (notePA e) a t (markupPA e)
    Rest d              -> Rest d
    Skip d              -> Skip d
    Chord ps d a t _    -> 
        NoteElem (Note middle_c d) a t (mconcat $ map markupF ps)

    Graces ns           -> Graces $ map notePA ns
    Punctuation s       -> Punctuation s
  where
    markupF                     = asMarkup mo

    notePA   :: Note pch drn -> (Note LyPitch drn)
    notePA (Note _ drn)         = Note middle_c drn

    markupPA :: Note pch drn -> Markup
    markupPA (Note pch _)       = markupF pch


--------------------------------------------------------------------------------
-- Output

rhythmicMarkupScore :: LyOutputDef pch anno 
                    -> ScoreInfo 
                    -> VoiceInfo -> GenLyPhrase pch anno -> Doc
rhythmicMarkupScore def infos infov ph =
        header $+$ simultaneous1 (rhythmicMarkupVoice def infov ph)
  where
    header          = scoreHeader infos


rhythmicMarkupVoice :: LyOutputDef pch anno 
                    -> VoiceInfo
                    -> GenLyPhrase pch anno -> Doc
rhythmicMarkupVoice def info ph = 
    block (Just rhythmic_staff) (modeBlockF $ (notes_header $+$ notes))
  where
    rhythmic_staff  = command "new" <+> text "RhythmicStaff"
    modeBlockF      = octaveModeBlock (voice_ly_octave_mode info)
    local1          = maybe default_local_info id $ firstContextInfo ph
    notes_header    = oPhraseHeader local1
    notes           = renderNotes def ph


oPhraseHeader :: LocalContextInfo -> Doc
oPhraseHeader locals = 
        key   (local_key locals)
    $+$ meter (local_meter locals)

-- TODO - note appropriate for RhythmicStaff etc.
--
octaveModeBlock :: OctaveMode -> Doc -> Doc
octaveModeBlock (AbsPitch)   d  = absolute $+$ d
octaveModeBlock (RelPitch p) d  = block (Just $ relative p) d

