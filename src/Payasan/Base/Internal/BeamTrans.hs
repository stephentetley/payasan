{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.BeamTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Main syntax to Beam syntax prior to translation
-- to ABC or LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.BeamTrans
  (
    transAndBeam
  ) where



import Payasan.Base.Internal.AddBeams
import qualified Payasan.Base.Internal.BeamSyntax as T
import Payasan.Base.Internal.MainSyntax

import Payasan.Base.Duration


transAndBeam :: (drn -> Duration) -> Phrase pch drn -> T.Phrase pch Duration
transAndBeam fn ph = addBeams $ phraseT fn ph


phraseT :: (drn -> Duration) -> Phrase pch drn -> T.Phrase pch Duration
phraseT fn (Phrase bs)          = T.Phrase $ map (barT fn) bs


barT :: (drn -> Duration) -> Bar pch drn -> T.Bar pch Duration
barT fn (Bar info cs)           = T.Bar info $ map (ctxElementT fn) cs

ctxElementT :: (drn -> Duration) 
            -> CtxElement pch drn 
            -> T.CtxElement pch Duration
ctxElementT fn (Atom e)         = T.Atom $ elementT fn e
ctxElementT fn (Tuplet spec cs) = T.Tuplet spec $ map (ctxElementT fn) cs

elementT :: (drn -> Duration) -> Element pch drn -> T.Element pch Duration
elementT fn (NoteElem a)        = T.NoteElem $ noteT fn a
elementT fn (Rest d)            = T.Rest $ fn d
elementT fn (Chord ps d)        = T.Chord ps $ fn d
elementT fn (Graces ns)         = T.Graces $ map (noteT fn) ns


noteT :: (drn -> Duration) -> Note pch drn -> T.Note pch Duration
noteT fn (Note pch drn) = T.Note pch (fn drn)
