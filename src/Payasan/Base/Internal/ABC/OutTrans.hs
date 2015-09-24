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



import Payasan.Base.Internal.ABC.Spelling
import Payasan.Base.Internal.ABC.Syntax
import Payasan.Base.Internal.ABC.Utils

import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.BeamTraversals
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad


import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as PCH

import Data.Ratio (numerator, denominator)




translate :: Phrase PCH.Pitch Duration anno -> Phrase Pitch NoteLength anno
translate = transformP pch_algo . transformD drn_algo

type PTMon a = Mon () a
type DTMon a = Mon UnitNoteLength a

--------------------------------------------------------------------------------
-- Pitch translation


-- TODO - This should be aware of keysig changes...


pch_algo :: BeamPitchAlgo () PCH.Pitch Pitch
pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = elementP
    }



elementP :: Element PCH.Pitch drn anno -> PTMon (Element Pitch drn anno)
elementP (NoteElem e a)         = (\e1 -> NoteElem e1 a) <$> noteP e
elementP (Rest d)               = pure $ Rest d
elementP (Chord ps d a)         = 
    (\ps1 -> Chord ps1 d a) <$> mapM transPch ps

elementP (Graces ns)            = Graces    <$> mapM noteP ns


noteP :: Note PCH.Pitch drn -> PTMon (Note Pitch drn)
noteP (Note pch drn)            = (\p -> Note p drn) <$> transPch pch

transPch :: PCH.Pitch -> PTMon Pitch
transPch p0 = 
    (\k -> let sm = makeSpellingMap k in spellFindNatural sm $ fromPitch p0) 
        <$> asksLocal local_key


--------------------------------------------------------------------------------
-- Translate duration

drn_algo :: BeamDurationAlgo UnitNoteLength Duration NoteLength
drn_algo = BeamDurationAlgo
    { initial_stateD    = UNIT_NOTE_8
    , element_trafoD    = elementD
    }



elementD :: Element pch Duration anno -> DTMon (Element pch NoteLength anno)
elementD (NoteElem e a)         = (\e1 -> NoteElem e1 a) <$> noteD e
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Chord ps d a)         = (\d1 -> Chord ps d1 a) <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns


noteD :: Note pch Duration -> DTMon (Note pch NoteLength)
noteD (Note pch drn)            = Note pch <$> changeDrn drn


changeDrn :: Duration -> DTMon NoteLength
changeDrn d                     = 
    (durationT `flip` d) <$> asksLocal local_unit_note_len


durationT :: UnitNoteLength -> Duration ->  NoteLength
durationT unl nd = 
    (fn . fork numerator denominator) $ (durationSize nd) / unitLength unl
  where  
    fork f g a = (f a, g a)
    fn (1,1)   = DNL
    fn (1,dn)  = Divd (fromIntegral dn)
    fn (nm,1)  = Mult (fromIntegral nm)
    fn (nm,dn) = Frac (fromIntegral nm) (fromIntegral dn)