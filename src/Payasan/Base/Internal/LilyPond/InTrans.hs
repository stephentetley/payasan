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

import Payasan.Base.Internal.BeamDurationTrafo as D
import Payasan.Base.Internal.BeamPitchTrafo as P
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as PCH



translate :: GlobalRenderInfo 
          -> Phrase Pitch NoteLength -> Phrase PCH.Pitch Duration 
translate info = pitchTrafo . D.transform drn_algo
  where
    -- If AbsPitch then /previous pitch/ will never be used
    pitchTrafo = case global_ly_octave_mode info of
                    RelPitch pch -> P.transform (rel_pch_algo pch)
                    AbsPitch -> P.transform abs_pch_algo


translateDurationOnly :: Phrase pch NoteLength -> Phrase pch Duration 
translateDurationOnly = D.transform drn_algo

type DTMon   a      = D.Mon Duration a
type RelPMon a      = D.Mon PCH.Pitch a
type AbsPMon a      = D.Mon () a



--------------------------------------------------------------------------------
-- Relative Pitch translation

rel_pch_algo :: PCH.Pitch -> P.BeamPitchAlgo PCH.Pitch Pitch PCH.Pitch
rel_pch_algo start = P.BeamPitchAlgo
    { P.initial_state           = start
    , P.element_trafo           = relElementP
    }


previousPitch :: RelPMon PCH.Pitch
previousPitch = get

setPrevPitch :: PCH.Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element Pitch drn -> RelPMon (Element PCH.Pitch drn)
relElementP (NoteElem a)        = NoteElem <$> relNoteP a
relElementP (Rest d)            = pure $ Rest d
relElementP (Chord ps d)        = (Chord `flip` d)  <$> mapM changePitchRel ps
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


abs_pch_algo :: P.BeamPitchAlgo () Pitch PCH.Pitch
abs_pch_algo = P.BeamPitchAlgo
    { P.initial_state           = ()
    , P.element_trafo           = absElementP
    }


absElementP :: Element Pitch drn -> AbsPMon (Element PCH.Pitch drn)
absElementP (NoteElem a)        = NoteElem <$> absNoteP a
absElementP (Rest d)            = pure $ Rest d
absElementP (Chord ps d)        = (Chord `flip` d)  <$> mapM changePitchAbs ps
absElementP (Graces ns)         = Graces <$> mapM absNoteP ns


absNoteP :: Note Pitch drn -> AbsPMon (Note PCH.Pitch drn)
absNoteP (Note pch drn)         = (\p -> Note p drn) <$> changePitchAbs pch


changePitchAbs :: Pitch -> AbsPMon PCH.Pitch
changePitchAbs p1 = return $ toPitchAbs p1




--------------------------------------------------------------------------------
-- Duration translation


drn_algo :: D.BeamDurationAlgo Duration NoteLength Duration 
drn_algo = D.BeamDurationAlgo
    { D.initial_state           = dQuarter
    , D.element_trafo           = elementD
    }

previousDuration :: DTMon Duration
previousDuration = get

setPrevDuration :: Duration -> DTMon ()
setPrevDuration d = put d


elementD :: Element pch NoteLength -> DTMon (Element pch Duration)
elementD (NoteElem a)           = NoteElem  <$> noteD a
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Chord ps d)           = Chord ps  <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns

noteD :: Note pch NoteLength -> DTMon (Note pch Duration)
noteD (Note pch drn)            = Note pch <$> changeDrn drn



changeDrn :: NoteLength -> DTMon Duration
changeDrn (DrnDefault)    = previousDuration
changeDrn (DrnExplicit d) = setPrevDuration d >> return d

