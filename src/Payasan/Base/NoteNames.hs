{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  SymNotelist.NoteNames
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pitch spelling for ABC.
--
--------------------------------------------------------------------------------

module SymNotelist.NoteNames
  (

    cflat, cnat, csharp
  , dflat, dnat, dsharp
  , eflat, enat, esharp
  , fflat, fnat, fsharp
  , gflat, gnat, gsharp
  , aflat, anat, asharp
  , bflat, bnat, bsharp

  ) where

import SymNotelist.Pitch


-- C 

cflat           :: NoteLabel
cflat           = NoteLabel C FLAT

cnat            :: NoteLabel
cnat            = NoteLabel C NAT

csharp          :: NoteLabel
csharp          = NoteLabel C SHARP

-- D

dflat           :: NoteLabel
dflat           = NoteLabel D FLAT

dnat            :: NoteLabel
dnat            = NoteLabel D NAT

dsharp          :: NoteLabel
dsharp          = NoteLabel D SHARP

-- E

eflat           :: NoteLabel
eflat           = NoteLabel E FLAT

enat            :: NoteLabel
enat            = NoteLabel E NAT

esharp          :: NoteLabel
esharp          = NoteLabel E SHARP

-- F 

fflat           :: NoteLabel
fflat           = NoteLabel F FLAT

fnat            :: NoteLabel
fnat            = NoteLabel F NAT

fsharp          :: NoteLabel
fsharp          = NoteLabel F SHARP

-- G 

gflat           :: NoteLabel
gflat           = NoteLabel G FLAT

gnat            :: NoteLabel
gnat            = NoteLabel G NAT

gsharp          :: NoteLabel
gsharp          = NoteLabel G SHARP

-- A

aflat           :: NoteLabel
aflat           = NoteLabel A FLAT

anat            :: NoteLabel
anat            = NoteLabel A NAT

asharp          :: NoteLabel
asharp          = NoteLabel A SHARP

-- B

bflat           :: NoteLabel
bflat           = NoteLabel B FLAT

bnat            :: NoteLabel
bnat            = NoteLabel B NAT

bsharp          :: NoteLabel
bsharp          = NoteLabel B SHARP
