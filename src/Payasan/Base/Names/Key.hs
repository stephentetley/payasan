{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Names.Key
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Named keys
--
--------------------------------------------------------------------------------

module Payasan.Base.Names.Key
  ( 

  -- * Key names
    c_major
  , g_major
  , d_major
  , a_major
  , e_major
  , b_major
  , f_sharp_major
  , c_sharp_major

  , f_major
  , b_flat_major
  , e_flat_major
  , a_flat_major
  , d_flat_major
  , g_flat_major
  , c_flat_major

  , a_minor
  , e_minor
  , b_minor
  , f_sharp_minor
  , c_sharp_minor
  , g_sharp_minor
  , d_sharp_minor
  , a_sharp_minor

  , d_minor
  , g_minor
  , c_minor
  , f_minor
  , b_flat_minor
  , e_flat_minor
  , a_flat_minor

  ) where

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Names.Pitch



--------------------------------------------------------------------------------
-- Key names

c_major             :: Key
c_major             = Key c_nat MAJOR


-- MAJOR with sharps

g_major             :: Key
g_major             = Key g_nat MAJOR

d_major             :: Key
d_major             = Key d_nat MAJOR

a_major             :: Key
a_major             = Key a_nat MAJOR

e_major             :: Key
e_major             = Key e_nat MAJOR

b_major             :: Key
b_major             = Key b_nat MAJOR

f_sharp_major       :: Key
f_sharp_major       = Key f_sharp MAJOR

c_sharp_major       :: Key
c_sharp_major       = Key c_sharp MAJOR



-- MAJOR with flats

f_major             :: Key
f_major             = Key f_nat MAJOR

b_flat_major        :: Key
b_flat_major        = Key b_flat MAJOR

e_flat_major        :: Key
e_flat_major        = Key e_flat MAJOR

a_flat_major        :: Key
a_flat_major        = Key a_flat MAJOR

d_flat_major        :: Key
d_flat_major        = Key d_flat MAJOR

g_flat_major        :: Key
g_flat_major        = Key g_flat MAJOR

c_flat_major        :: Key
c_flat_major        = Key c_flat MAJOR


-- MINOR keys

a_minor             :: Key
a_minor             = Key a_nat MINOR

-- MINOR with sharps
e_minor             :: Key
e_minor             = Key e_nat MINOR

b_minor             :: Key
b_minor             = Key b_nat MINOR

f_sharp_minor       :: Key
f_sharp_minor       = Key f_sharp MINOR

c_sharp_minor       :: Key
c_sharp_minor       = Key c_sharp MINOR

g_sharp_minor       :: Key
g_sharp_minor       = Key g_sharp MINOR

d_sharp_minor       :: Key
d_sharp_minor       = Key d_sharp MINOR

a_sharp_minor       :: Key
a_sharp_minor       = Key a_sharp MINOR



-- MINOR with flats

d_minor          :: Key
d_minor          = Key d_nat MINOR

g_minor          :: Key
g_minor          = Key g_nat MINOR

c_minor          :: Key
c_minor          = Key c_nat MINOR

f_minor          :: Key
f_minor          = Key f_nat MINOR

b_flat_minor     :: Key
b_flat_minor     = Key b_flat MINOR

e_flat_minor     :: Key
e_flat_minor     = Key e_flat MINOR

a_flat_minor     :: Key
a_flat_minor     = Key a_flat MINOR

