{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Plain
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- /Plain/ constructor syntax, not quasiquoted.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Plain
  ( 
    NoteListAtom
  , fromNoteList
  , note
  , rest

  ) where


import Payasan.Base.Internal.MainSyntax
import Payasan.Base.Internal.CommonSyntax

import Payasan.Base.Duration
import Payasan.Base.Pitch

type NoteListAtom = NoteGroup Pitch Duration ()

fromNoteList :: SectionInfo -> [[NoteListAtom]] -> Part Pitch Duration ()
fromNoteList ri xss = Part $ map (Bar ri) xss

note :: Pitch -> Duration -> NoteListAtom
note p d = Atom $ NoteElem (Note p d) () NO_TIE

rest :: Duration -> NoteListAtom
rest d = Atom $ Rest d 