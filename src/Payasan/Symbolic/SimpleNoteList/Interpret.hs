{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Symbolic.SimpleNoteList.Interpret
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Render...
--
--------------------------------------------------------------------------------

module Payasan.Symbolic.SimpleNoteList.Interpret
  ( 
    renderPhrase
  , phrase
  ) where

import Payasan.Symbolic.SimpleNoteList.Base

import Payasan.Base
import Payasan.Base.Advance

renderPhrase :: NoteF -> TrackData -> Phrase a -> Track
renderPhrase fn cfg phz = renderAdvance cfg $ getPhrase phz fn

-- | A beat in Djembe system is an eighth not a quarter note
--
phrase :: Motif -> Phrase ()
phrase patt = Phrase $ \tf -> mapM_ (render1 tf ) $ extrMotif patt 
    

render1 :: NoteF -> Element -> Advance ()
render1 _  (Rest d)   = interpNoteLength d >>= \drn -> advanceCursor drn
render1 nf (Note p d) = let pch = toPitch p in
                        interpNoteLength d >>= \drn -> event_ drn (nf pch)

interpNoteLength :: (Monad m, ContextM m) => NoteLength -> m Seconds
interpNoteLength len = 
    default_note_length >>= \dflt -> durationInSeconds (dflt * toBeat len)