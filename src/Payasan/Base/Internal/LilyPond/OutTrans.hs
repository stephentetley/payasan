{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert pitch and duration to their LilyPond equivalents 
-- prior to printing.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.OutTrans
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
          -> Phrase PCH.Pitch Duration anno 
          -> Phrase Pitch NoteLength anno
translate info = pitchTrafo . transformD drn_algo
  where
    -- If AbsPitch then /previous pitch/ will never be used
    pitchTrafo = case global_ly_octave_mode info of
                    RelPitch pch -> transformP (rel_pch_algo pch)
                    AbsPitch -> transformP abs_pch_algo



translateDurationOnly :: Phrase pch Duration anno 
                      -> Phrase pch NoteLength anno
translateDurationOnly = transformD drn_algo

type DTMon   a      = Mon Duration a
type RelPMon a      = Mon PCH.Pitch a
type AbsPMon a      = Mon () a


--------------------------------------------------------------------------------
-- Relative Pitch translation

rel_pch_algo :: PCH.Pitch -> BeamPitchAlgo PCH.Pitch PCH.Pitch Pitch
rel_pch_algo start = BeamPitchAlgo
    { initial_stateP    = start
    , element_trafoP    = relElementP
    }


previousPitch :: RelPMon PCH.Pitch
previousPitch = get

setPrevPitch :: PCH.Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element PCH.Pitch drn anno -> RelPMon (Element Pitch drn anno)
relElementP (NoteElem e a)      = (\e1 -> NoteElem e1 a) <$> relNoteP e
relElementP (Rest d)            = pure $ Rest d
relElementP (Chord ps d a)      = 
    (\ps1 -> Chord ps1 d a) <$> mapM changePitchRel ps

relElementP (Graces ns)         = Graces <$> mapM relNoteP ns


relNoteP :: Note PCH.Pitch drn -> RelPMon (Note Pitch drn)
relNoteP (Note pch drn)         = (\p -> Note p drn) <$> changePitchRel pch


changePitchRel :: PCH.Pitch -> RelPMon Pitch
changePitchRel p1 = 
    do { tp0 <- previousPitch
       ; let tp1 = fromPitchRel p1 tp0
       ; setPrevPitch p1
       ; return tp1
       }

--------------------------------------------------------------------------------
-- Abs Pitch translation


abs_pch_algo :: BeamPitchAlgo () PCH.Pitch Pitch
abs_pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = absElementP
    }


absElementP :: Element PCH.Pitch drn anno -> AbsPMon (Element Pitch drn anno)
absElementP (NoteElem e a)      = (\e1 -> NoteElem e1 a) <$> absNoteP e
absElementP (Rest d)            = pure $ Rest d
absElementP (Chord ps d a)      = 
    (\ps1 -> Chord ps1 d a)  <$> mapM changePitchAbs ps

absElementP (Graces ns)         = Graces <$> mapM absNoteP ns


absNoteP :: Note PCH.Pitch drn -> AbsPMon (Note Pitch drn)
absNoteP (Note pch drn)         = (\p -> Note p drn) <$> changePitchAbs pch


-- No previous pitch indicates Absolute pitch mode
changePitchAbs :: PCH.Pitch -> AbsPMon Pitch
changePitchAbs p1 = return $ fromPitchAbs p1


--------------------------------------------------------------------------------
-- Duration translation


drn_algo :: BeamDurationAlgo Duration Duration NoteLength
drn_algo = BeamDurationAlgo
    { initial_stateD    = dQuarter
    , element_trafoD    = elementD
    }

previousDuration :: DTMon Duration
previousDuration = get

setPrevDuration :: Duration -> DTMon ()
setPrevDuration d = put d


elementD :: Element pch Duration anno -> DTMon (Element pch NoteLength anno)
elementD (NoteElem e a)         = (\e1 -> NoteElem  e1 a) <$> noteD e
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Chord ps d a)         = (\d1 -> Chord ps d1 a) <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns

noteD :: Note pch Duration -> DTMon (Note pch NoteLength)
noteD (Note pch drn)            = Note pch <$> changeDrn drn



changeDrn :: Duration -> DTMon NoteLength
changeDrn d1 =
   do { d0 <- previousDuration 
      ; if d1 == d0 
          then return DrnDefault
          else setPrevDuration d1 >> return (DrnExplicit d1)
      }


