{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Cadenza.Internal.CadenzaToBeam
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Cadenza syntax to Beam syntax prior to outputting 
-- LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Cadenza.Internal.CadenzaToBeam
  (
    translateToBeam
  ) where


import Payasan.LilyPond.Cadenza.Internal.Syntax

import qualified Payasan.Base.Internal.BeamSyntax as T


translateToBeam :: Phrase pch drn anno -> T.Phrase pch drn anno
translateToBeam                 = phraseT


phraseT ::Phrase pch drn anno -> T.Phrase pch drn anno
phraseT (Phrase info gs)        = T.Phrase [T.Bar info $ map noteGroupT gs]



noteGroupT :: NoteGroup pch drn anno -> T.NoteGroup pch drn anno
noteGroupT (Atom e)             = T.Atom $ elementT e
noteGroupT (Beamed gs)          = T.Beamed $ map noteGroupT gs
noteGroupT (Tuplet spec gs)     = T.Tuplet spec $ map noteGroupT gs

elementT :: Element pch drn anno -> T.Element pch drn anno
elementT (Note p d a t)         = T.NoteElem (T.Note p d) a t
elementT (Rest d)               = T.Rest d
elementT (Spacer d)             = T.Spacer d
elementT (Skip d)               = T.Skip d
elementT (Punctuation s)        = T.Punctuation s

