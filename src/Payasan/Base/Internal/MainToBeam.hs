{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.MainToBeam
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Main syntax to Beam syntax prior to outputting
-- ABC or LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.MainToBeam
  (
    translateToBeam
  ) where



import qualified Payasan.Base.Internal.BeamSyntax as T
import Payasan.Base.Internal.MainSyntax



translateToBeam :: Phrase pch drn -> T.Phrase pch drn
translateToBeam                 = phraseT


phraseT :: Phrase pch drn -> T.Phrase pch drn
phraseT (Phrase bs)             = T.Phrase $ map barT bs


barT :: Bar pch drn -> T.Bar pch drn
barT (Bar info cs)              = T.Bar info $ map noteGroupT cs

noteGroupT :: NoteGroup pch drn -> T.NoteGroup pch drn
noteGroupT (Atom e)             = T.Atom $ elementT e
noteGroupT (Tuplet spec cs)     = T.Tuplet spec $ map noteGroupT cs

elementT :: Element pch drn -> T.Element pch drn
elementT (NoteElem a)           = T.NoteElem $ noteT a
elementT (Rest d)               = T.Rest d
elementT (Chord ps d)           = T.Chord ps d
elementT (Graces ns)            = T.Graces $ map noteT ns


noteT :: Note pch drn -> T.Note pch drn
noteT (Note pch drn)            = T.Note pch drn
