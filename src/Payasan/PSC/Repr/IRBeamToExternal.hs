{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Repr.PSC.IRBeamToExternal
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Beam syntax to Main syntax after parsing.
--
-- WARNING / TODO - this module should be redundant. Concrete syntax
-- should parse to external.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IRBeamToExternal
  (
    translateToMain
  ) where


import Payasan.PSC.Repr.IRBeam.Syntax
import qualified Payasan.PSC.Repr.External.Syntax as T



translateToMain :: Part pch drn anno -> T.Part pch drn anno
translateToMain                 = partT


partT :: Part pch drn anno -> T.Part pch drn anno
partT (Part bs)                 = T.Part $ map barT bs



barT :: Bar pch drn anno  -> T.Bar pch drn anno
barT (Bar info cs)              = T.Bar info $ concatMap noteGroupT cs
       


-- | Remember - a beamed NoteGroup may generate 1+ elements
--
noteGroupT :: NoteGroup pch drn anno -> [T.NoteGroup pch drn anno]
noteGroupT (Atom e)             = [T.Atom $ elementT e]
noteGroupT (Tuplet spec cs)     = [T.Tuplet spec $ concatMap noteGroupT cs]
noteGroupT (Beamed cs)          = concatMap noteGroupT cs



elementT :: Element pch drn anno  -> T.Element pch drn anno
elementT (NoteElem e a t)       = T.NoteElem (noteT e) a t
elementT (Rest d)               = T.Rest d 
elementT (Spacer d)             = T.Spacer d 
elementT (Skip d)               = T.Skip d 
elementT (Chord ps d a t)       = T.Chord ps d a t
elementT (Graces ns)            = T.Graces $ map noteT ns
elementT (Punctuation s)        = T.Punctuation s


noteT :: Note pch drn -> T.Note pch drn
noteT (Note pch drn)            = T.Note pch drn

