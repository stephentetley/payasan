{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.External.ABCInTrans
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert ABC to Main Syntax, plus /pushing/ render info
-- into bars as this cannot be done during parsing / 
-- quasiquoting.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.External.ABCInTrans
  (
    unquoteABC
  , translateFromInput
  ) where

import Payasan.PSC.Base.ABCCommon

import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.PSC.Base.RewriteMonad
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Scale


type PTMon a = Mon () a
type DTMon a = Mon UnitNoteLength a


unquoteABC :: String -> SectionInfo -> ABCSectionQuote -> Section Pitch Duration ()
unquoteABC name info (ABCSectionQuote bs) =
    let unl = section_unit_note_len info
        bars = translateDuration unl $ translatePitch bs
    in Section { section_name      = name
               , section_info      = info
               , section_bars      = bars
               }

-- | DEPRECATED - input should be translated from ABCSectionQuote
translateFromInput :: Part ABCPitch ABCNoteLength anno 
                   -> Part Pitch Duration anno
translateFromInput = transformP pch_algo . transformD drn_algo


--------------------------------------------------------------------------------
-- Pitch translation


-- | DEPRECATED
pch_algo :: BeamPitchAlgo () ABCPitch Pitch
pch_algo = BeamPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = elementP
    }

-- | DEPRECATED
drn_algo :: BeamDurationAlgo UnitNoteLength ABCNoteLength Duration
drn_algo = BeamDurationAlgo
    { initial_stateD    = UNIT_NOTE_8
    , element_trafoD    = elementD
    }

--------------------------------------------------------------------------------
-- Pitch translation

translatePitch :: [Bar ABCPitch drn anno] 
               -> [Bar Pitch drn anno]
translatePitch = genTransformBars elementP () 

   

elementP :: Element ABCPitch drn anno -> PTMon (Element Pitch drn anno)
elementP (NoteElem e a t)       = (\e1 -> NoteElem e1 a t)  <$> noteP e
elementP (Rest d)               = pure $ Rest d
elementP (Spacer d)             = pure $ Spacer d
elementP (Skip d)               = pure $ Skip d
elementP (Chord ps d a t)       = 
    (\ps1 -> Chord ps1 d a t) <$> mapM transPch ps

elementP (Graces ns)            = Graces    <$> mapM noteP ns
elementP (Punctuation s)        = pure $ Punctuation s


noteP :: Note ABCPitch drn -> PTMon (Note Pitch drn)
noteP (Note pch drn)            = (\p -> Note p drn) <$> transPch pch



-- Pitches might be /natural/ in the score when the are
-- actually sharpened or flattened according to key 
-- signature

transPch :: ABCPitch -> PTMon Pitch
transPch p0 = (\k -> toPitch (buildScale k) p0) <$> asks section_key



--------------------------------------------------------------------------------
-- Translate duration


translateDuration :: UnitNoteLength 
                  -> [Bar pch ABCNoteLength anno] 
                  -> [Bar pch Duration anno]
translateDuration unl = genTransformBars elementD unl 


elementD :: Element pch ABCNoteLength anno -> DTMon (Element pch Duration anno)
elementD (NoteElem e a t)       = (\e1 -> NoteElem e1 a t) <$> noteD e
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Spacer d)             = Spacer    <$> changeDrn d
elementD (Skip d)               = Skip      <$> changeDrn d
elementD (Chord ps d a t)       = (\d1 -> Chord ps d1 a t) <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM noteD ns
elementD (Punctuation s)        = pure $ Punctuation s


noteD :: Note pch ABCNoteLength -> DTMon (Note pch Duration)
noteD (Note pch drn)            = Note pch <$> changeDrn drn


changeDrn :: ABCNoteLength -> DTMon Duration
changeDrn d                     = 
    (\unl -> toDuration unl d) <$> asks section_unit_note_len


