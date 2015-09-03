{-# LANGUAGE DeriveDataTypeable         #-}
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
-- Convert Bracket syntax to ABC prior to printing.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABC.OutTrans
  (
    translate
  ) where



import qualified Payasan.Base.Internal.ABC.Syntax as T
import Payasan.Base.Internal.ABC.Utils

import Payasan.Base.Internal.BeamSyntax

import Payasan.Base.Duration
import Payasan.Base.Pitch (Pitch)

import Data.Ratio



translate :: Phrase Pitch Duration -> T.ABCPhrase
translate ph = phraseT ph



phraseT :: Phrase Pitch Duration -> T.ABCPhrase
phraseT (Phrase bs)             = T.ABCPhrase $ map barT bs

-- | At this point acquire default note length.
--
barT :: Bar Pitch Duration -> T.Bar
barT (Bar info cs)              = 
    let f = fromPitch           -- might need /context/ i.e. key
        g = durationT (local_unit_note_len info)
    in T.Bar info $ map (ctxElementT f g) cs

ctxElementT :: (pch -> T.Pitch) -> (Duration -> T.NoteLength) 
            -> CtxElement pch Duration -> T.CtxElement
ctxElementT f g (Atom e)            = T.Atom $ elementT f g e
ctxElementT f g (Tuplet spec cs)    = T.Tuplet spec $ map (ctxElementT f g) cs
ctxElementT f g (Beamed cs)         = T.Beamed $ map (ctxElementT f g) cs



elementT :: (pch -> T.Pitch) -> (Duration -> T.NoteLength) 
         -> Element pch Duration -> T.Element
elementT f g (NoteElem a)       = T.NoteElem $ noteT f g a
elementT _ g (Rest d)           = T.Rest (g d)
elementT f g (Chord ps d)       = T.Chord (map f ps) (g d)
elementT f g (Graces ns)        = T.Graces $ map (noteT f g) ns


noteT :: (pch -> T.Pitch) -> (Duration -> T.NoteLength) 
      -> Note pch Duration -> T.Note
noteT f g (Note pch drn)        = T.Note (f pch) (g drn)



durationT :: UnitNoteLength -> Duration -> T.NoteLength
durationT unl nd = 
    (fn . fork numerator denominator) $ (durationSize nd) / unitLength unl
  where  
    fork f g a = (f a, g a)
    fn (1,1)   = T.DNL
    fn (1,dn)  = T.Divd (fromIntegral dn)
    fn (nm,1)  = T.Mult (fromIntegral nm)
    fn (nm,dn) = T.Frac (fromIntegral nm) (fromIntegral dn)