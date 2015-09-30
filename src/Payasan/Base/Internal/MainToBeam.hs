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



translateToBeam :: Phrase pch drn anno -> T.Phrase pch drn anno
translateToBeam                 = phraseT


phraseT :: Phrase pch drn anno -> T.Phrase pch drn anno
phraseT (Phrase bs)             = T.Phrase $ map barT bs


barT :: Bar pch drn anno -> T.Bar pch drn anno
barT (Bar info cs)              = T.Bar info $ map noteGroupT cs

noteGroupT :: NoteGroup pch drn anno -> T.NoteGroup pch drn anno
noteGroupT (Atom e)             = T.Atom $ elementT e
noteGroupT (Tuplet spec cs)     = T.Tuplet spec $ map noteGroupT cs

elementT :: Element pch drn anno -> T.Element pch drn anno
elementT (NoteElem e a)         = T.NoteElem (noteT e) a
elementT (Rest d)               = T.Rest d
elementT (Chord ps d a)         = T.Chord ps d a
elementT (Graces ns)            = T.Graces $ map noteT ns
elementT (Punctuation s)        = T.Punctuation s


noteT :: Note pch drn -> T.Note pch drn
noteT (Note pch drn)            = T.Note pch drn
