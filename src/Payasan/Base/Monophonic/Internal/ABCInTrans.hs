{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.ABCInTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert ABC to Monophonic syntax.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.ABCInTrans
  (
    abcTranslate
  ) where



import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Monophonic.Internal.Traversals

import Payasan.Base.Internal.ABC.Spelling
import Payasan.Base.Internal.ABC.Syntax (ABCPitch, ABCNoteLength)
import Payasan.Base.Internal.ABC.Utils
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration
import Payasan.Base.Pitch


abcTranslate :: ABCMonoPhrase -> Phrase Pitch Duration ()
abcTranslate = transformP pch_algo . transformD drn_algo


--------------------------------------------------------------------------------
-- Pitch translation

type PMon a = Mon () a

pch_algo :: MonoPitchAlgo () ABCPitch Pitch
pch_algo = MonoPitchAlgo
    { initial_stateP    = ()
    , element_trafoP    = elementP
    }


elementP :: Element ABCPitch drn anno -> PMon (Element Pitch drn anno) 
elementP (Note p d a)           = (\p1 -> Note p1 d a) <$> transPch p
elementP (Rest d)               = pure $ Rest d



transPch :: ABCPitch -> PMon Pitch
transPch p0 = 
    (\k -> let sm = makeSpellingMap k in toPitch $ spellFindAlteration sm p0) 
        <$> asksLocal local_key

--------------------------------------------------------------------------------
-- Translate duration

type DMon a = Mon UnitNoteLength a

drn_algo :: MonoDurationAlgo UnitNoteLength ABCNoteLength Duration
drn_algo = MonoDurationAlgo
    { initial_stateD    = UNIT_NOTE_8
    , element_trafoD    = elementD
    }


elementD :: Element pch ABCNoteLength anno -> DMon (Element pch Duration anno)
elementD (Note p d a)           = (\d1 -> Note p d1 a) <$> changeDrn d
elementD (Rest d)               = Rest    <$> changeDrn d


changeDrn :: ABCNoteLength -> DMon Duration
changeDrn d = (durationT `flip` d) <$> asksLocal local_unit_note_len


durationT :: UnitNoteLength -> ABCNoteLength -> Duration
durationT unl d = 
    let rat = rduration unl d in case rationalToDuration rat of
      Nothing -> d_longa
      Just ans -> ans

