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
  , translate
  ) where



import Payasan.Base.Internal.LilyPond.OutTrans hiding ( translate )
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals
import Payasan.Base.Internal.CommonSyntax

import Payasan.Base.Duration


data MarkupOutput pch = MarkupOutput { asMarkup :: pch -> Markup }


translate :: GlobalRenderInfo 
          -> MarkupOutput pch
          -> Phrase pch Duration anno 
          -> Phrase LyPitch LyNoteLength Markup
translate _   mo = 
    transformPA (markup_algo mo) . translateDurationOnly



type PAMon    a     = Mon () a


--------------------------------------------------------------------------------
-- Pitch to markup translation

markup_algo :: MarkupOutput pch -> BeamPitchAnnoAlgo () pch a LyPitch Markup
markup_algo mo = BeamPitchAnnoAlgo
    { initial_statePA   = ()
    , element_trafoPA   = elementPA mo
    }



elementPA :: forall pch drn anno. 
             MarkupOutput pch 
          -> Element pch drn anno 
          -> PAMon (Element LyPitch drn Markup)
elementPA mo elt = case elt of 
    NoteElem e _        -> NoteElem <$> notePA e <*> markupPA e
    Rest d              -> pure $ Rest d
    Chord ps d _        -> 
        (\p annos -> NoteElem (Note p d) (mconcat annos)) 
            <$> middleC <*> pure (map markupF ps)

    Graces ns           -> Graces <$> mapM notePA ns
    Punctuation s       -> pure $ Punctuation s
  where
    markupF                     = asMarkup mo

    notePA   :: Note pch drn -> PAMon (Note LyPitch drn)
    notePA (Note _ drn)         = (\p -> Note p drn) <$> middleC

    markupPA :: Note pch drn -> PAMon Markup
    markupPA (Note pch _)       = pure $ markupF pch

    middleC :: PAMon LyPitch
    middleC                     = pure middle_c

