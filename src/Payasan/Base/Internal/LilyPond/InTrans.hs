{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.InTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert LilyPond to Main Syntax.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.InTrans
  (
    translate
  , translateDurationOnly
  ) where


import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as PCH



translate :: GlobalRenderInfo 
          -> Phrase Pitch NoteLength anno 
          -> Phrase PCH.Pitch Duration anno
translate info = pitchTrafo . transformD drn_algo
  where
    -- If AbsPitch then /previous pitch/ will never be used
    pitchTrafo = case global_ly_octave_mode info of
                    RelPitch pch -> transformP (rel_pch_algo pch)
                    AbsPitch -> transformP abs_pch_algo


translateDurationOnly :: Phrase pch NoteLength anno 
                      -> Phrase pch Duration anno
translateDurationOnly = transformD drn_algo

type DTMon   a      = Mon Duration a
type RelPMon a      = Mon PCH.Pitch a
type AbsPMon a      = Mon () a



--------------------------------------------------------------------------------
-- Relative Pitch translation

rel_pch_algo :: PCH.Pitch -> BeamPitchAlgo PCH.Pitch Pitch PCH.Pitch
rel_pch_algo start = BeamPitchAlgo
    { initial_stateP    = start
    , element_trafoP    = relElementP
    }


previousPitch :: RelPMon PCH.Pitch
previousPitch = get

setPrevPitch :: PCH.Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element Pitch drn anno -> RelPMon (Element PCH.Pitch drn anno)
relElementP (NoteElem e a)      = (\e1 -> NoteElem e1 a) <$> relNoteP e
relElementP (Rest d)            = pure $ Rest d
relElementP (Chord ps d a)      = 
    (\ps1 -> Chord ps1 d a) <$> mapM changePitchRel ps

relElementP (Graces ns)         = Graces <$> mapM relNoteP ns


relNoteP :: Note Pitch drn -> RelPMon (Note PCH.Pitch drn)
relNoteP (Note pch drn)         = (\p -> Note p drn) <$> changePitchRel pch


changePitchRel :: Pitch -> RelPMon PCH.Pitch
changePitchRel p1 = 
    do { p0 <- previousPitch
       ; let tp1 = toPitchRel p1 p0
       ; setPrevPitch tp1
       ; return tp1
       }
                              



--------------------------------------------------------------------------------
-- Abs Pitch translation


abs_pch_algo :: BeamPitchAlgo () Pitch PCH.Pitch
abs_pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = absElementP
    }


absElementP :: Element Pitch drn anno -> AbsPMon (Element PCH.Pitch drn anno)
absElementP (NoteElem e a)      = (\e1 -> NoteElem e1 a) <$> absNoteP e
absElementP (Rest d)            = pure $ Rest d
absElementP (Chord ps d a)      = 
    (\ps1 -> Chord ps1 d a) <$> mapM changePitchAbs ps

absElementP (Graces ns)         = Graces <$> mapM absNoteP ns


absNoteP :: Note Pitch drn -> AbsPMon (Note PCH.Pitch drn)
absNoteP (Note pch drn)         = (\p -> Note p drn) <$> changePitchAbs pch


changePitchAbs :: Pitch -> AbsPMon PCH.Pitch
changePitchAbs p1 = return $ toPitchAbs p1




--------------------------------------------------------------------------------
-- Duration translation


drn_algo :: BeamDurationAlgo Duration NoteLength Duration 
drn_algo = BeamDurationAlgo
    { initial_stateD    = dQuarter
    , element_trafoD    = elementD
    }

previousDuration :: DTMon Duration
previousDuration = get

setPrevDuration :: Duration -> DTMon ()
setPrevDuration d = put d


elementD :: Element pch NoteLength anno -> DTMon (Element pch Duration anno)
elementD (NoteElem e a)         = (\e1 -> NoteElem e1 a) <$> noteD e
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Chord ps d a)         = (\d1 -> Chord ps d1 a) <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns

noteD :: Note pch NoteLength -> DTMon (Note pch Duration)
noteD (Note pch drn)            = Note pch <$> changeDrn drn



changeDrn :: NoteLength -> DTMon Duration
changeDrn (DrnDefault)    = previousDuration
changeDrn (DrnExplicit d) = setPrevDuration d >> return d

