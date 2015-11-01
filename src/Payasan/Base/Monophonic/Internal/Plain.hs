{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monomorphic.Internal.Plain
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- /Plain/ constructor syntax, not quasiquoted.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.Plain
  ( 
    NoteListAtom
  , fromNoteList
  , note
  , rest

  ) where


import Payasan.Base.Monophonic.Internal.RecalcBars
import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Internal.CommonSyntax

import Payasan.Base.Duration
import Payasan.Base.Pitch

type NoteListAtom = StdMonoNoteGroup

fromNoteList :: LocalContextInfo -> [NoteListAtom] -> StdMonoPhrase
fromNoteList ri xs = recalcBars $ Phrase ri [Bar xs]

note :: Pitch -> Duration -> NoteListAtom
note p d = Atom $ Note p d () NO_TIE

rest :: Duration -> NoteListAtom
rest d = Atom $ Rest d 