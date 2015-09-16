{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.MonoToMain
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Monophonic syntax to Main syntax (pipline output 
-- from Main syntax).
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.MonoToMain
  (
    translateToMain
  ) where



import qualified Payasan.Base.Internal.MainSyntax as T
import Payasan.Base.Monophonic.Internal.Syntax



translateToMain :: Phrase pch drn anno -> T.Phrase pch drn anno
translateToMain = phraseT


phraseT :: Phrase pch drn anno -> T.Phrase pch drn anno
phraseT (Phrase bs)             = T.Phrase $ map barT bs


barT :: Bar pch drn anno -> T.Bar pch drn anno
barT (Bar info cs)              = T.Bar info $ concatMap noteGroupT cs


-- | Remember - a beamed NoteGroup may generate 1+ elements
--
noteGroupT :: NoteGroup pch drn anno -> [T.NoteGroup pch drn anno]
noteGroupT (Atom e)             = [T.Atom $ elementT e]
noteGroupT (Tuplet spec cs)     = [T.Tuplet spec $ concatMap noteGroupT cs]


elementT :: Element pch drn anno -> T.Element pch drn anno
elementT (Note p d a)           = T.NoteElem (T.Note p d) a
elementT (Rest d)               = T.Rest d
