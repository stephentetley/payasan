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
                          -> Phrase LyPitch LyNoteLength Markup
translateToRhythmicMarkup mo = 
    transformPA (markup_algo mo) . translateToOutput_DurationOnly




--------------------------------------------------------------------------------
-- Pitch to markup translation

markup_algo :: MarkupOutput pch -> BeamPitchAnnoAlgo () pch anno LyPitch Markup
markup_algo mo = BeamPitchAnnoAlgo
    { initial_statePA   = ()
    , element_trafoPA   = liftElementTrafo $ elementP mo
    }


elementP :: forall pch drn anno. 
            MarkupOutput pch 
         -> Element pch drn anno 
         -> Element LyPitch drn Markup
elementP mo elt = case elt of 
    NoteElem e _ t      -> NoteElem (notePA e) (markupPA e) t
    Rest d              -> Rest d
    Spacer d            -> Spacer d
    Skip d              -> Skip d
    Chord ps d _ t      -> 
        NoteElem (Note middle_c d) (mconcat $ map markupF ps) t

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
                    -> LyPhrase2 pch anno -> Doc
rhythmicMarkupScore def infos ph =
        header $+$ simultaneous1 (rhythmicMarkupVoice def ph)
  where
    header          = scoreHeader infos


rhythmicMarkupVoice :: LyOutputDef pch anno 
                    -> LyPhrase2 pch anno -> Doc
rhythmicMarkupVoice def ph = 
    block (Just newRhythmicStaff_) (absolute_ $+$ notes_header $+$ notes)
  where
    local1          = maybe default_local_info id $ firstContextInfo ph
    notes_header    = oPhraseHeader local1
    notes           = lilypondNotes def local1 ph


oPhraseHeader :: LocalContextInfo -> Doc
oPhraseHeader locals = 
        key_  (local_key locals)
    $+$ time_ (local_meter locals)


