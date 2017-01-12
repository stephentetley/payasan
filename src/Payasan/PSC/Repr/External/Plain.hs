{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.External.Plain
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- /Plain/ constructor syntax, not quasiquoted.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.External.Plain
  ( 
    NoteListAtom
  , fromNoteList
  , note
  , rest

  ) where


import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch

type NoteListAtom = NoteGroup Pitch Duration ()

fromNoteList :: String -> SectionInfo -> [[NoteListAtom]] -> Part Pitch Duration ()
fromNoteList name info xss = Part [Section name info $ map Bar xss]

note :: Pitch -> Duration -> NoteListAtom
note p d = Atom $ Note p d () NO_TIE

rest :: Duration -> NoteListAtom
rest d = Atom $ Rest d 