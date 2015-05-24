{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Names.Duration
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Simple NoteList
--
--------------------------------------------------------------------------------

module Payasan.Base.Names.Duration
  ( 

    dwhole
  , dhalf
  , dquarter
  , deighth
  , dsixteenth

  , d'1
  , d'2
  , d'4
  , d'8
  , d'16

  ) where


import Payasan.Base.Internal.Base

dwhole          :: Beat
dwhole          = 4



dhalf           :: Beat
dhalf           = 2

dquarter        :: Beat
dquarter        = 1

deighth         :: Beat 
deighth         = 0.5


dsixteenth      :: Beat
dsixteenth      = 0.25



d'1             :: Beat 
d'1             = dwhole

d'2             :: Beat 
d'2             = dhalf

d'4             :: Beat 
d'4             = dquarter

d'8             :: Beat
d'8             = deighth

d'16            :: Beat
d'16            = dsixteenth