{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert pitch and duration to their ABC equivalents prior 
-- to printing.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABC.OutTrans
  (
    translateToOutput
  ) where



import Payasan.Base.Internal.ABC.Syntax

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad
import Payasan.Base.Internal.Scale


import Payasan.Base.Duration
import Payasan.Base.Pitch





translateToOutput :: Phrase Pitch Duration anno -> GenABCPhrase anno
translateToOutput = transformP pch_algo . transformD drn_algo

type PTMon a = Mon () a
type DTMon a = Mon UnitNoteLength a

--------------------------------------------------------------------------------
-- Pitch translation


-- TODO - This should be aware of keysig changes...


pch_algo :: BeamPitchAlgo () Pitch ABCPitch
pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = elementP
    }



elementP :: Element Pitch drn anno -> PTMon (Element ABCPitch drn anno)
elementP (NoteElem e a t m)     = (\e1 -> NoteElem e1 a t m) <$> noteP e
elementP (Rest d)               = pure $ Rest d
elementP (Skip d)               = pure $ Skip d
elementP (Chord ps d a t m)     = 
    (\ps1 -> Chord ps1 d a t m) <$> mapM transPch ps

elementP (Graces ns)            = Graces    <$> mapM noteP ns
elementP (Punctuation s)        = pure $ Punctuation s


noteP :: Note Pitch drn -> PTMon (Note ABCPitch drn)
noteP (Note pch drn)            = (\p -> Note p drn) <$> transPch pch

-- This should be optimized to not make a scale each time!
--
transPch :: Pitch -> PTMon ABCPitch
transPch p0 = (\k -> fromPitch (buildScale k) p0) <$> asksLocal local_key

--------------------------------------------------------------------------------
-- Translate duration

drn_algo :: BeamDurationAlgo UnitNoteLength Duration ABCNoteLength
drn_algo = BeamDurationAlgo
    { initial_stateD    = UNIT_NOTE_8
    , element_trafoD    = elementD
    }



-- Skip is just a Rest to ABC
--
elementD :: Element pch Duration anno -> DTMon (Element pch ABCNoteLength anno)
elementD (NoteElem e a t m)     = (\e1 -> NoteElem e1 a t m) <$> noteD e
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Skip d)               = Skip      <$> changeDrn d
elementD (Chord ps d a t m)     = (\d1 -> Chord ps d1 a t m) <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns
elementD (Punctuation s)        = pure $ Punctuation s


noteD :: Note pch Duration -> DTMon (Note pch ABCNoteLength)
noteD (Note pch drn)            = Note pch <$> changeDrn drn


changeDrn :: Duration -> DTMon ABCNoteLength
changeDrn d                     = 
    (\unl -> fromDuration unl d) <$> asksLocal local_unit_note_len


