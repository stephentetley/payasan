{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.BeamToMain
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Beam syntax to Main syntax after parsing.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.BeamToMain
  (
    translateToMain
  ) where


import Payasan.Base.Internal.BeamSyntax
import qualified Payasan.Base.Internal.MainSyntax as T



translateToMain :: Phrase pch drn -> T.Phrase pch drn
translateToMain                 = phraseT


phraseT :: Phrase pch drn -> T.Phrase pch drn
phraseT (Phrase bs)          = T.Phrase $ map barT bs



barT :: Bar pch drn  -> T.Bar pch drn
barT (Bar info cs)              = T.Bar info $ concatMap noteGroupT cs
       


-- | Remember - a beamed NoteGroup may generate 1+ elements
--
noteGroupT :: NoteGroup pch drn -> [T.NoteGroup pch drn]
noteGroupT (Atom e)             = [T.Atom $ elementT e]
noteGroupT (Tuplet spec cs)     = [T.Tuplet spec $ concatMap noteGroupT cs]
noteGroupT (Beamed cs)          = concatMap noteGroupT cs



elementT :: Element pch drn  -> T.Element pch drn
elementT (NoteElem a)           = T.NoteElem $ noteT a
elementT (Rest d)               = T.Rest d 
elementT (Chord ps d)           = T.Chord ps d
elementT (Graces ns)            = T.Graces $ map noteT ns


noteT :: Note pch drn -> T.Note pch drn
noteT (Note pch drn)            = T.Note pch drn

