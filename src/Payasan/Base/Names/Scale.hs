{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Names.Key
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Named scales
--
--------------------------------------------------------------------------------

module Payasan.Base.Names.Scale
  ( 

  -- * Key names
    c_major_scale
  , g_major_scale
  , d_major_scale
  , a_major_scale
  , e_major_scale
  , b_major_scale
  , f_sharp_major_scale
  , c_sharp_major_scale

  , f_major_scale
  , b_flat_major_scale
  , e_flat_major_scale
  , a_flat_major_scale
  , d_flat_major_scale
  , g_flat_major_scale
  , c_flat_major_scale

  , a_minor_scale
  , e_minor_scale
  , b_minor_scale
  , f_sharp_minor_scale
  , c_sharp_minor_scale
  , g_sharp_minor_scale
  , d_sharp_minor_scale
  , a_sharp_minor_scale

  , d_minor_scale
  , g_minor_scale
  , c_minor_scale
  , f_minor_scale
  , b_flat_minor_scale
  , e_flat_minor_scale
  , a_flat_minor_scale

  ) where

import Payasan.Base.Internal.Scale
import Payasan.Base.Names.Key



--------------------------------------------------------------------------------
-- Key names

c_major_scale           :: Scale
c_major_scale           = buildScale c_major


-- MAJOR with sharps

g_major_scale           :: Scale
g_major_scale           = buildScale g_major

d_major_scale           :: Scale
d_major_scale           = buildScale d_major

a_major_scale           :: Scale
a_major_scale           = buildScale a_major

e_major_scale           :: Scale
e_major_scale           = buildScale e_major

b_major_scale           :: Scale
b_major_scale           = buildScale b_major

f_sharp_major_scale     :: Scale
f_sharp_major_scale     = buildScale f_sharp_major

c_sharp_major_scale     :: Scale
c_sharp_major_scale     = buildScale c_sharp_major



-- major with flats

f_major_scale           :: Scale
f_major_scale           = buildScale f_major

b_flat_major_scale      :: Scale
b_flat_major_scale      = buildScale b_flat_major

e_flat_major_scale      :: Scale
e_flat_major_scale      = buildScale e_flat_major

a_flat_major_scale      :: Scale
a_flat_major_scale      = buildScale a_flat_major

d_flat_major_scale      :: Scale
d_flat_major_scale      = buildScale d_flat_major

g_flat_major_scale      :: Scale
g_flat_major_scale      = buildScale g_flat_major

c_flat_major_scale      :: Scale
c_flat_major_scale       = buildScale c_flat_major


--_minor scales

a_minor_scale           :: Scale
a_minor_scale             = buildScale a_minor

--_minor with sharps
e_minor_scale             :: Scale
e_minor_scale             = buildScale e_minor

b_minor_scale             :: Scale
b_minor_scale             = buildScale b_minor

f_sharp_minor_scale       :: Scale
f_sharp_minor_scale       = buildScale f_sharp_minor

c_sharp_minor_scale       :: Scale
c_sharp_minor_scale       = buildScale c_sharp_minor

g_sharp_minor_scale       :: Scale
g_sharp_minor_scale       = buildScale g_sharp_minor

d_sharp_minor_scale       :: Scale
d_sharp_minor_scale       = buildScale d_sharp_minor

a_sharp_minor_scale       :: Scale
a_sharp_minor_scale       = buildScale a_sharp_minor



-- minor with flats

d_minor_scale          :: Scale
d_minor_scale          = buildScale d_minor

g_minor_scale          :: Scale
g_minor_scale          = buildScale g_minor

c_minor_scale          :: Scale
c_minor_scale          = buildScale c_minor

f_minor_scale          :: Scale
f_minor_scale          = buildScale f_minor

b_flat_minor_scale     :: Scale
b_flat_minor_scale     = buildScale b_flat_minor

e_flat_minor_scale     :: Scale
e_flat_minor_scale     = buildScale e_flat_minor

a_flat_minor_scale      :: Scale
a_flat_minor_scale      = buildScale a_flat_minor

