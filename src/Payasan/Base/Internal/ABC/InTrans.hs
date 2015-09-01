{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.InTrans
-- Copyright   :  (c) Stephen Tetley 2015
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

module Payasan.Base.Internal.ABC.InTrans
  (
    translate
  , pushLocalRenderInfo
  ) where



import Payasan.Base.Internal.ABC.Syntax
import Payasan.Base.Internal.ABC.Utils
import qualified Payasan.Base.Internal.MainSyntax as T

import Payasan.Base.Duration
import qualified Payasan.Base.Pitch as T



translate :: ABCPhrase -> T.Phrase T.Pitch Duration
translate = phraseT


phraseT :: ABCPhrase -> T.Phrase T.Pitch Duration
phraseT (ABCPhrase bs)          = T.Phrase $ map barT bs


barT :: Bar  -> T.Bar T.Pitch Duration
barT (Bar info cs)              = 
    let f = durationT (local_unit_note_len info) 
    in T.Bar info $ concatMap (ctxElementT f) cs


-- | Remember - a beamed CtxElement may generate 1+ elements
--
ctxElementT :: (NoteLength -> Duration) 
            -> CtxElement -> [T.CtxElement T.Pitch Duration]
ctxElementT f (Atom e)         = [T.Atom $ elementT f e]
ctxElementT f (Tuplet spec cs) = [T.Tuplet spec $ concatMap (ctxElementT f) cs]
ctxElementT f (Beamed cs)      = concatMap (ctxElementT f) cs


elementT :: (NoteLength -> Duration) 
         -> Element  -> T.Element T.Pitch Duration
elementT f (NoteElem a)       = T.NoteElem $ noteT f a
elementT f (Rest d)           = T.Rest (f d)
elementT f (Chord ps d)       = T.Chord (map pitchT ps) (f d)
elementT f (Graces ns)        = T.Graces $ map (noteT f) ns


noteT :: (NoteLength -> Duration) 
      -> Note -> T.Note T.Pitch Duration
noteT f (Note pch drn)        = T.Note (pitchT pch) (f drn)


pitchT :: Pitch -> T.Pitch
pitchT = toPitch

durationT :: UnitNoteLength -> NoteLength -> Duration
durationT unl d = 
    let rat = rduration unl d in case rationalToDuration rat of
      Nothing -> dLonga
      Just ans -> ans


--------------------------------------------------------------------------------
-- Push RenderInfo into bars.


pushLocalRenderInfo :: LocalRenderInfo -> ABCPhrase -> ABCPhrase
pushLocalRenderInfo ri (ABCPhrase bs) = ABCPhrase $ map upd bs
  where
    upd bar = bar { render_info = ri }
