{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.LilyPond.RhythmicMarkup
-- Copyright   :  (c) Stephen Tetley 2015-2017
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

module Payasan.PSC.LilyPond.RhythmicMarkup
  (
    MarkupOutput(..)
  , translateToLyPartOut_RhythmicMarkup

  , rhythmicMarkupScore
  , rhythmicMarkupVoice

  ) where


import Payasan.PSC.LilyPond.Base
import Payasan.PSC.LilyPond.OutTrans
import Payasan.PSC.LilyPond.Pretty
import Payasan.PSC.LilyPond.SimpleOutput

import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils

import Payasan.Base.Basis
import Payasan.Base.Duration


import Text.PrettyPrint.HughesPJ        -- package: pretty



data MarkupOutput pch = MarkupOutput { asMarkup :: pch -> Doc }


translateToLyPartOut_RhythmicMarkup :: MarkupOutput pch
                                    -> Part pch Duration anno 
                                    -> Part LyPitch LyNoteLength Doc
translateToLyPartOut_RhythmicMarkup mo = 
    transformExternal (markup_algo mo) . translateToLyPartOut_DurationOnly




--------------------------------------------------------------------------------
-- Pitch to markup translation

markup_algo :: MarkupOutput pch -> ExternalAlgo () pch LyPitch drn drn anno Doc
markup_algo mo = ExternalAlgo
    { initial_state     = ()
    , element_trafo     = liftElementTrafo $ elementP mo
    }


elementP :: forall pch drn anno. 
            MarkupOutput pch 
         -> Element pch drn anno 
         -> Element LyPitch drn Doc
elementP mo elt = case elt of 
    Note p d _ t        -> Note middle_c d (markupF p) t
    Rest d              -> Rest d
    Spacer d            -> Spacer d
    Skip d              -> Skip d
    Chord ps d _ t      -> 
        Note middle_c d (mconcat $ map markupF ps) t

    Graces ns           -> Graces $ map grace1PA ns
    Punctuation s       -> Punctuation s
  where
    markupF                     = asMarkup mo

    grace1PA :: Grace1 pch drn -> (Grace1 LyPitch drn)
    grace1PA (Grace1 _ drn)     = Grace1 middle_c drn



--------------------------------------------------------------------------------
-- Output

-- TODO - (Simple) Rhythmic output only differs from regular 
-- tonal output in (b) the context embedding the notelist
-- and (b) not printing a key signature. 
--
-- Ideally we would have user (library designer) specified
-- SectionInfo but this would add complexity.

rhythmicMarkupScore :: LyOutputDef pch anno 
                    -> String 
                    -> String
                    -> Part pch LyNoteLength anno -> Doc
rhythmicMarkupScore def lyversion title ph =
        header $+$ simultaneous1 (rhythmicMarkupVoice def ph)
  where
    header          = scoreHeader lyversion title


rhythmicMarkupVoice :: LyOutputDef pch anno 
                    -> Part pch LyNoteLength anno -> Doc
rhythmicMarkupVoice def ph = 
    block (Just newRhythmicStaff_) 
          (absolute_ $+$ notes_header $+$ (extractDoc notes))
  where
    local1          = initialSectionInfo ph
    notes_header    = renderPartHeader local1
    notes           = makeLyNoteList def ph



-- TODO - this should be common...
renderPartHeader :: SectionInfo -> Doc
renderPartHeader locals = case section_meter locals of
    Unmetered -> maybe empty id keyline
    Metered t -> keyline ?+$ time_ t
  where
    keyline = key_  <$> section_key locals
