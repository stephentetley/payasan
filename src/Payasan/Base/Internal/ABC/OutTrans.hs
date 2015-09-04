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
    translate
  ) where



import Payasan.Base.Internal.ABC.Syntax
import Payasan.Base.Internal.ABC.Utils

import Payasan.Base.Internal.BeamDurationTrafo as D
import Payasan.Base.Internal.BeamPitchTrafo as P
import Payasan.Base.Internal.Utils


import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as PCH

import Data.Ratio (numerator, denominator)




translate :: Phrase PCH.Pitch Duration -> Phrase Pitch NoteLength
translate = P.transform pch_algo . D.transform drn_algo

type DTMon a = D.Mon UnitNoteLength a
type PTMon a = D.Mon () a

--------------------------------------------------------------------------------
-- Pitch translation


-- TODO - This should be aware of keysig changes...

pch_algo :: P.BeamPitchAlgo () PCH.Pitch Pitch
pch_algo = P.BeamPitchAlgo
    { P.initial_state           = ()
    , P.bar_info_action         = actionInfoP
    , P.element_trafo           = elementP
    }


actionInfoP :: LocalRenderInfo -> PTMon ()
actionInfoP _ = return ()

elementP :: Element PCH.Pitch drn -> PTMon (Element Pitch drn)
elementP (NoteElem a)           = NoteElem  <$> noteP a
elementP (Rest d)               = pure $ Rest d
elementP (Chord ps d)           = (Chord `flip` d) <$> mapM transPch ps
elementP (Graces ns)            = Graces    <$> mapM noteP ns


noteP :: Note PCH.Pitch drn -> PTMon (Note Pitch drn)
noteP (Note pch drn)            = (\p -> Note p drn) <$> transPch pch

-- likely to change wrt key sig...
transPch :: PCH.Pitch -> PTMon Pitch
transPch = pure . fromPitch


--------------------------------------------------------------------------------
-- Translate duration

drn_algo :: D.BeamDurationAlgo UnitNoteLength Duration NoteLength
drn_algo = D.BeamDurationAlgo
    { D.initial_state           = UNIT_NOTE_8
    , D.bar_info_action         = actionInfoD
    , D.element_trafo           = elementD
    }


actionInfoD :: LocalRenderInfo -> DTMon ()
actionInfoD info = put (local_unit_note_len info)

elementD :: Element pch Duration -> DTMon (Element pch NoteLength)
elementD (NoteElem a)           = NoteElem  <$> noteD a
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Chord ps d)           = Chord ps  <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns


noteD :: Note pch Duration -> DTMon (Note pch NoteLength)
noteD (Note pch drn)            = Note pch <$> changeDrn drn


changeDrn :: Duration -> DTMon NoteLength
changeDrn d                     = (durationT `flip` d) <$> get

durationT :: UnitNoteLength -> Duration ->  NoteLength
durationT unl nd = 
    (fn . fork numerator denominator) $ (durationSize nd) / unitLength unl
  where  
    fork f g a = (f a, g a)
    fn (1,1)   = DNL
    fn (1,dn)  = Divd (fromIntegral dn)
    fn (nm,1)  = Mult (fromIntegral nm)
    fn (nm,dn) = Frac (fromIntegral nm) (fromIntegral dn)