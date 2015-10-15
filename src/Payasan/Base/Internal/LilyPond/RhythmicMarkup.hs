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

  , rhythmicMarkupOutput

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

markup_algo :: MarkupOutput pch -> BeamPitchAnnoAlgo () pch a LyPitch Markup
markup_algo mo = BeamPitchAnnoAlgo
    { initial_statePA   = ()
    , element_trafoPA   = liftElementTrafo $ elementPA mo
    }


elementPA :: forall pch drn anno. 
             MarkupOutput pch 
          -> Element pch drn anno 
          -> Element LyPitch drn Markup
elementPA mo elt = case elt of 
    NoteElem e _        -> NoteElem (notePA e) (markupPA e)
    Rest d              -> Rest d
    Chord ps d _        -> 
        NoteElem (Note middle_c d) (mconcat $ map markupF ps)

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

rhythmicMarkupOutput :: LyOutputDef pch anno 
                     -> ScoreInfo 
                     -> GenLyPhrase pch anno -> Doc
rhythmicMarkupOutput def info ph =
        header
    $+$ simultaneous1 (block (Just rhythmic_staff) 
                             (modeBlockF $ (notes_header $+$ notes)))
  where
    local1          = maybe default_local_info id $ firstContextInfo ph
    header          = oHeader info
    modeBlockF      = octaveModeBlock (global_ly_octave_mode info)
    rhythmic_staff  = command "new" <+> text "RhythmicStaff"
    notes_header    = oPhraseHeader local1
    notes           = renderNotes def ph


oHeader :: ScoreInfo -> Doc
oHeader globals = 
        version (global_ly_version globals)
    $+$ block (Just $ command "header") (title $ global_title globals)


-- TODO - note appropriate for RhythmicStaff etc.
--
oPhraseHeader :: LocalContextInfo -> Doc
oPhraseHeader locals = 
        key   (local_key locals)
    $+$ meter (local_meter locals)

-- TODO - note appropriate for RhythmicStaff etc.
--
octaveModeBlock :: OctaveMode -> Doc -> Doc
octaveModeBlock (AbsPitch)   d  = absolute $+$ d
octaveModeBlock (RelPitch p) d  = block (Just $ relative p) d

