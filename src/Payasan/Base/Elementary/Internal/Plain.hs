{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.Plain
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

module Payasan.Base.Elementary.Internal.Plain
  ( 
    NoteListAtom
  , fromNoteList
  , note
  , rest

  ) where


import Payasan.Base.Elementary.Internal.RecalcBars
import Payasan.Base.Elementary.Internal.Syntax
import Payasan.Base.Internal.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch

type NoteListAtom = StdElemNoteGroup

fromNoteList :: SectionInfo -> [NoteListAtom] -> StdElemPart
fromNoteList ri xs = recalcBars $ Part ri [Bar xs]

note :: Pitch -> Duration -> NoteListAtom
note p d = Atom $ Note p d () NO_TIE

rest :: Duration -> NoteListAtom
rest d = Atom $ Rest d 