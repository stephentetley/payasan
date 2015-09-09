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
  ) where



import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import Payasan.Base.Internal.BeamDurationTrafo as D
import Payasan.Base.Internal.BeamPitchTrafo as P
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as PCH



translate :: GlobalRenderInfo 
          -> Phrase PCH.Pitch Duration -> Phrase Pitch NoteLength
translate info = pitchTrafo . D.transform drn_algo
  where
    -- If AbsPitch then /previous pitch/ will never be used
    pitchTrafo = case global_pitch_directive info of
                    RelPitch pch -> P.transform (rel_pch_algo pch)
                    AbsPitch -> P.transform abs_pch_algo


type DTMon a = D.Mon Duration a
type RelPMon a = D.Mon PCH.Pitch a
type AbsPMon a = D.Mon () a


--------------------------------------------------------------------------------
-- Relative Pitch translation

rel_pch_algo :: PCH.Pitch -> P.BeamPitchAlgo PCH.Pitch PCH.Pitch Pitch
rel_pch_algo start = P.BeamPitchAlgo
    { P.initial_state           = start
    , P.bar_info_action         = \_ -> return ()
    , P.element_trafo           = relElementP
    }


previousPitch :: RelPMon PCH.Pitch
previousPitch = get

setPrevPitch :: PCH.Pitch -> RelPMon ()
setPrevPitch = put 


relElementP :: Element PCH.Pitch drn -> RelPMon (Element Pitch drn)
relElementP (NoteElem a)        = NoteElem <$> relNoteP a
relElementP (Rest d)            = pure $ Rest d
relElementP (Chord ps d)        = (Chord `flip` d)  <$> mapM changePitchRel ps
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


abs_pch_algo :: P.BeamPitchAlgo () PCH.Pitch Pitch
abs_pch_algo = P.BeamPitchAlgo
    { P.initial_state           = ()
    , P.bar_info_action         = \_ -> return ()
    , P.element_trafo           = absElementP
    }


absElementP :: Element PCH.Pitch drn -> AbsPMon (Element Pitch drn)
absElementP (NoteElem a)        = NoteElem <$> absNoteP a
absElementP (Rest d)            = pure $ Rest d
absElementP (Chord ps d)        = (Chord `flip` d)  <$> mapM changePitchAbs ps
absElementP (Graces ns)         = Graces <$> mapM absNoteP ns


absNoteP :: Note PCH.Pitch drn -> AbsPMon (Note Pitch drn)
absNoteP (Note pch drn)         = (\p -> Note p drn) <$> changePitchAbs pch


-- No previous pitch indicates Absolute pitch mode
changePitchAbs :: PCH.Pitch -> AbsPMon Pitch
changePitchAbs p1 = return $ fromPitchAbs p1


--------------------------------------------------------------------------------
-- Duration translation


drn_algo :: D.BeamDurationAlgo Duration Duration NoteLength
drn_algo = D.BeamDurationAlgo
    { D.initial_state           = dQuarter
    , D.bar_info_action         = \_ -> return ()
    , D.element_trafo           = elementD
    }

previousDuration :: DTMon Duration
previousDuration = get

setPrevDuration :: Duration -> DTMon ()
setPrevDuration d = put d


elementD :: Element pch Duration -> DTMon (Element pch NoteLength)
elementD (NoteElem a)           = NoteElem  <$> noteD a
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Chord ps d)           = Chord ps  <$> changeDrn d
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


