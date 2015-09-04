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
barT (Bar info cs)              = T.Bar info $ concatMap ctxElementT cs
       


-- | Remember - a beamed CtxElement may generate 1+ elements
--
ctxElementT :: CtxElement pch drn -> [T.CtxElement pch drn]
ctxElementT (Atom e)            = [T.Atom $ elementT e]
ctxElementT (Tuplet spec cs)    = [T.Tuplet spec $ concatMap ctxElementT cs]
ctxElementT (Beamed cs)         = concatMap ctxElementT cs



elementT :: Element pch drn  -> T.Element pch drn
elementT (NoteElem a)           = T.NoteElem $ noteT a
elementT (Rest d)               = T.Rest d 
elementT (Chord ps d)           = T.Chord ps d
elementT (Graces ns)            = T.Graces $ map noteT ns


noteT :: Note pch drn -> T.Note pch drn
noteT (Note pch drn)            = T.Note pch drn

