{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Symbolic.NoteList
-- Copyright   :  (c) Stephen Tetley 2014
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Simple NoteList
--
--------------------------------------------------------------------------------

module Payasan.Symbolic.NoteList
  ( 

    Note(..)
  , GenEvent
  , phrase

  ) where


import Payasan.Base hiding ( Chord(..) )
import Payasan.Base.Advance




-- | At some point make this parametric on pitch...
--
data Note = Note  Pitch   Beat
          | Chord [Pitch] Beat
          | Rest  Beat 
  deriving (Eq,Ord,Show)


-- Pitch
type GenEvent impl = Pitch -> Event impl
-- 


-- Potentially Base.NoteList should use Seconds rather than 
-- Beats...

phrase :: GenEvent impl -> [Note] -> Advance ()
phrase mf xs = mapM_ fn  xs
  where
    fn (Note p d)   = durationInSeconds d >>= \dsecs ->
                      event_ dsecs (mf p)

    fn (Chord ps d) = durationInSeconds d >>= \dsecs ->
                      mapM_ (\pch -> eventNoWidth dsecs (mf pch)) ps >>
                      advanceCursor dsecs

    fn (Rest d)     = durationInSeconds d >>= \dsecs ->
                      advanceCursor dsecs

