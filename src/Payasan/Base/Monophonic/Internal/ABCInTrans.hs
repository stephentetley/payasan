{-# LANGUAGE DeriveDataTypeable         #-}
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
-- Convert ABC to Monophonic Syntax, plus /pushing/ render info
-- into bars as this cannot be done during parsing / 
-- quasiquoting.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.ABCInTrans
  (
    translate
  , pushRenderInfo
  ) where



import Payasan.Base.Monophonic.Internal.MonoSyntax

import Payasan.Base.Internal.ABCUtils
import Payasan.Base.Internal.ABCSyntax (NoteLength(..))

import Payasan.Base.Duration
import Payasan.Base.Pitch



translate :: ABCMonoPhrase -> StdMonoPhrase
translate = phraseT


phraseT :: ABCMonoPhrase -> StdMonoPhrase
phraseT (MonoPhrase bs)          = MonoPhrase $ map barT bs


barT :: Bar Pitch NoteLength -> Bar Pitch Duration
barT (Bar info cs)              = 
    let f = durationT (render_unit_note_len info) 
    in Bar info $ concatMap (ctxElementT f) cs


-- | Remember - a beamed CtxElement may generate 1+ elements
--
ctxElementT :: (NoteLength -> Duration) 
            -> CtxElement Pitch NoteLength -> [CtxElement Pitch Duration]
ctxElementT f (Atom e)         = [Atom $ elementT f e]
ctxElementT f (Tuplet spec cs) = [Tuplet spec $ concatMap (ctxElementT f) cs]


elementT :: (NoteLength -> Duration) 
         -> Element Pitch NoteLength  -> Element Pitch Duration
elementT f (Note p d)         = Note p (f d)
elementT f (Rest d)           = Rest (f d)


durationT :: UnitNoteLength -> NoteLength -> Duration
durationT unl d = 
    let rat = rduration unl d in case rationalToDuration rat of
      Nothing -> dLonga
      Just ans -> ans


--------------------------------------------------------------------------------
-- Push RenderInfo into bars.


pushRenderInfo :: RenderInfo -> ABCMonoPhrase -> ABCMonoPhrase
pushRenderInfo ri (MonoPhrase bs) = MonoPhrase $ map upd bs
  where
    upd bar = bar { render_info = ri }
