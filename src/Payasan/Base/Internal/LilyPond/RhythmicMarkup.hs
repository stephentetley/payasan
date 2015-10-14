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
  ) where



import Payasan.Base.Internal.LilyPond.OutTrans
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals

import Payasan.Base.Duration


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

