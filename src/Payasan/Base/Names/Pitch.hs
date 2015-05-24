{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Names.Pitch
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Named pitches (ET12)
--
--------------------------------------------------------------------------------

module Payasan.Base.Names.Pitch
  ( 


  -- * Pitch constructors
    middle_c
  
  , cf_1, c_1, cs_1, df_1, d_1, ds_1, ef_1, e_1, es_1
  , ff_1, f_1, fs_1, gf_1, g_1, gs_1, af_1, a_1, as_1
  , bf_1, b_1, bs_1  

  , cf_2, c_2, cs_2, df_2, d_2, ds_2, ef_2, e_2, es_2
  , ff_2, f_2, fs_2, gf_2, g_2, gs_2, af_2, a_2, as_2
  , bf_2, b_2, bs_2  

  , cf_3, c_3, cs_3, df_3, d_3, ds_3, ef_3, e_3, es_3
  , ff_3, f_3, fs_3, gf_3, g_3, gs_3, af_3, a_3, as_3
  , bf_3, b_3, bs_3  

  , cf_4, c_4, cs_4, df_4, d_4, ds_4, ef_4, e_4, es_4
  , ff_4, f_4, fs_4, gf_4, g_4, gs_4, af_4, a_4, as_4
  , bf_4, b_4, bs_4  

  , cf_5, c_5, cs_5, df_5, d_5, ds_5, ef_5, e_5, es_5
  , ff_5, f_5, fs_5, gf_5, g_5, gs_5, af_5, a_5, as_5
  , bf_5, b_5, bs_5  

  , cf_6, c_6, cs_6, df_6, d_6, ds_6, ef_6, e_6, es_6
  , ff_6, f_6, fs_6, gf_6, g_6, gs_6, af_6, a_6, as_6
  , bf_6, b_6, bs_6  

  , cf_7, c_7, cs_7, df_7, d_7, ds_7, ef_7, e_7, es_7
  , ff_7, f_7, fs_7, gf_7, g_7, gs_7, af_7, a_7, as_7
  , bf_7, b_7, bs_7  

  , cf_8, c_8, cs_8, df_8, d_8, ds_8, ef_8, e_8, es_8
  , ff_8, f_8, fs_8, gf_8, g_8, gs_8, af_8, a_8, as_8
  , bf_8, b_8, bs_8 


  ) where

import Payasan.Base.Internal.Pitch



--------------------------------------------------------------------------------
-- Pitches


makeC           :: Int -> Pitch
makeC ove       = makePitch ove 0 

makeD           :: Int -> Pitch
makeD ove       = makePitch ove 2

makeE            :: Int -> Pitch
makeE ove       = makePitch ove 4

makeF           :: Int -> Pitch
makeF ove       = makePitch ove 5

makeG           :: Int -> Pitch
makeG ove       = makePitch ove 7

makeA           :: Int -> Pitch
makeA ove       = makePitch ove 9

makeB           :: Int -> Pitch
makeB ove       = makePitch ove 11




middle_c        :: Pitch
middle_c        = c_4 



-- Octave 5

cf_1            :: Pitch
cf_1            = flatten $ makeC 5

c_1             :: Pitch
c_1             = makeC 5 

cs_1            :: Pitch
cs_1            = sharpen $ makeC 5

df_1            :: Pitch
df_1            = flatten $ makeD 5

d_1             :: Pitch
d_1             = makeD 5

ds_1            :: Pitch
ds_1            = sharpen $ makeD 5 

ef_1            :: Pitch
ef_1            = flatten $ makeE 5

e_1             :: Pitch
e_1             = makeE 5

es_1            :: Pitch
es_1            = sharpen $ makeE 5

ff_1            :: Pitch
ff_1            = flatten $ makeF 5

f_1             :: Pitch
f_1             = makeF 5

fs_1            :: Pitch
fs_1            = sharpen $ makeF 5

gf_1            :: Pitch
gf_1            = flatten $ makeG 5

g_1             :: Pitch
g_1             = makeG 5

gs_1            :: Pitch
gs_1            = sharpen $ makeG 5

af_1            :: Pitch
af_1            = flatten $ makeA 5

a_1             :: Pitch
a_1             = makeA 5

as_1            :: Pitch
as_1            = sharpen $ makeA 5

bf_1            :: Pitch
bf_1            = flatten $ makeB 5

b_1             :: Pitch
b_1             = makeB 5

bs_1            :: Pitch 
bs_1            = sharpen $ makeB 5


-- Octave 6

cf_2            :: Pitch
cf_2            = flatten $ makeC 6

c_2             :: Pitch
c_2             = makeC 6

cs_2            :: Pitch
cs_2            = sharpen $ makeC 6

df_2            :: Pitch
df_2            = flatten $ makeD 6

d_2             :: Pitch
d_2             = makeD 6

ds_2            :: Pitch
ds_2            = sharpen $ makeD 6

ef_2            :: Pitch
ef_2            = flatten $ makeE 6

e_2             :: Pitch
e_2             = makeE 6

es_2            :: Pitch
es_2            = sharpen $ makeE 6

ff_2            :: Pitch
ff_2            = flatten $ makeF 6

f_2             :: Pitch
f_2             = makeF 6

fs_2            :: Pitch
fs_2            = sharpen $ makeF 6

gf_2            :: Pitch
gf_2            = flatten $ makeG 6

g_2             :: Pitch
g_2             = makeG 6

gs_2            :: Pitch
gs_2            = sharpen $ makeG 6

af_2            :: Pitch
af_2            = flatten $ makeA 6

a_2             :: Pitch
a_2             = makeA 6

as_2            :: Pitch
as_2            = sharpen $ makeA 6

bf_2            :: Pitch
bf_2            = flatten $ makeB 6

b_2             :: Pitch
b_2             = makeB 6

bs_2            :: Pitch 
bs_2            = sharpen $ makeB 6



-- Octave 7

cf_3            :: Pitch
cf_3            = flatten $ makeC 7

c_3             :: Pitch
c_3             = makeC 7

cs_3            :: Pitch
cs_3            = sharpen $ makeC 7

df_3            :: Pitch
df_3            = flatten $ makeD 7

d_3             :: Pitch
d_3             = makeD 7

ds_3            :: Pitch
ds_3            = sharpen $ makeD 7

ef_3            :: Pitch
ef_3            = flatten $ makeE 7

e_3             :: Pitch
e_3             = makeE 7

es_3            :: Pitch
es_3            = sharpen $ makeE 7

ff_3            :: Pitch
ff_3            = flatten $ makeF 7

f_3             :: Pitch
f_3             = makeF 7

fs_3            :: Pitch
fs_3            = sharpen $ makeF 7

gf_3            :: Pitch
gf_3            = flatten $ makeG 7

g_3             :: Pitch
g_3             = makeG 7

gs_3            :: Pitch
gs_3            = sharpen $ makeG 7

af_3            :: Pitch
af_3            = flatten $ makeA 7

a_3             :: Pitch
a_3             = makeA 7

as_3            :: Pitch
as_3            = sharpen $ makeA 7

bf_3            :: Pitch
bf_3            = flatten $ makeB 7

b_3             :: Pitch
b_3             = makeB 7

bs_3            :: Pitch 
bs_3            = sharpen $ makeB 7


-- Octave 8

cf_4            :: Pitch
cf_4            = flatten $ makeC 8

c_4             :: Pitch
c_4             = makeC 8

cs_4            :: Pitch
cs_4            = sharpen $ makeC 8

df_4            :: Pitch
df_4            = flatten $ makeD 8

d_4             :: Pitch
d_4             = makeD 8

ds_4            :: Pitch
ds_4            = sharpen $ makeD 8

ef_4            :: Pitch
ef_4            = flatten $ makeE 8

e_4             :: Pitch
e_4             = makeE 8

es_4            :: Pitch
es_4            = sharpen $ makeE 8

ff_4            :: Pitch
ff_4            = flatten $ makeF 8

f_4             :: Pitch
f_4             = makeF 8

fs_4            :: Pitch
fs_4            = sharpen $ makeF 8

gf_4            :: Pitch
gf_4            = flatten $ makeG 8

g_4             :: Pitch
g_4             = makeG 8

gs_4            :: Pitch
gs_4            = sharpen $ makeG 8

af_4            :: Pitch
af_4            = flatten $ makeA 8

a_4             :: Pitch
a_4             = makeA 8

as_4            :: Pitch
as_4            = sharpen $ makeA 8

bf_4            :: Pitch
bf_4            = flatten $ makeB 8

b_4             :: Pitch
b_4             = makeB 8

bs_4            :: Pitch 
bs_4            = sharpen $ makeB 8


-- Octave 9

cf_5            :: Pitch
cf_5            = flatten $ makeC 9

c_5             :: Pitch
c_5             = makeC 9

cs_5            :: Pitch
cs_5            = sharpen $ makeC 9

df_5            :: Pitch
df_5            = flatten $ makeD 9

d_5             :: Pitch
d_5             = makeD 9

ds_5            :: Pitch
ds_5            = sharpen $ makeD 9

ef_5            :: Pitch
ef_5            = flatten $ makeE 9

e_5             :: Pitch
e_5             = makeE 9

es_5            :: Pitch
es_5            = sharpen $ makeE 9

ff_5            :: Pitch
ff_5            = flatten $ makeF 9

f_5             :: Pitch
f_5             = makeF 9

fs_5            :: Pitch
fs_5            = sharpen $ makeF 9

gf_5            :: Pitch
gf_5            = flatten $ makeG 9

g_5             :: Pitch
g_5             = makeG 9

gs_5            :: Pitch
gs_5            = sharpen $ makeG 9

af_5            :: Pitch
af_5            = flatten $ makeA 9

a_5             :: Pitch
a_5             = makeA 9

as_5            :: Pitch
as_5            = sharpen $ makeA 9

bf_5            :: Pitch
bf_5            = flatten $ makeB 9

b_5             :: Pitch
b_5             = makeB 9

bs_5            :: Pitch 
bs_5            = sharpen $ makeB 9


-- Octave 10

cf_6            :: Pitch
cf_6            = flatten $ makeC 10

c_6             :: Pitch
c_6             = makeC 10

cs_6            :: Pitch
cs_6            = sharpen $ makeC 10

df_6            :: Pitch
df_6            = flatten $ makeD 10

d_6             :: Pitch
d_6             = makeD 10

ds_6            :: Pitch
ds_6            = sharpen $ makeD 10

ef_6            :: Pitch
ef_6            = flatten $ makeE 10

e_6             :: Pitch
e_6             = makeE 10

es_6            :: Pitch
es_6            = sharpen $ makeE 10

ff_6            :: Pitch
ff_6            = flatten $ makeF 10

f_6             :: Pitch
f_6             = makeF 10

fs_6            :: Pitch
fs_6            = sharpen $ makeF 10

gf_6            :: Pitch
gf_6            = flatten $ makeG 10

g_6             :: Pitch
g_6             = makeG 10

gs_6            :: Pitch
gs_6            = sharpen $ makeG 10

af_6            :: Pitch
af_6            = flatten $ makeA 10

a_6             :: Pitch
a_6             = makeA 10

as_6            :: Pitch
as_6            = sharpen $ makeA 10

bf_6            :: Pitch
bf_6            = flatten $ makeB 10

b_6             :: Pitch
b_6             = makeB 10

bs_6            :: Pitch 
bs_6            = sharpen $ makeB 10


-- Octave 11

cf_7            :: Pitch
cf_7            = flatten $ makeC 11

c_7             :: Pitch
c_7             = makeC 11

cs_7            :: Pitch
cs_7            = sharpen $ makeC 11

df_7            :: Pitch
df_7            = flatten $ makeD 11

d_7             :: Pitch
d_7             = makeD 11

ds_7            :: Pitch
ds_7            = sharpen $ makeD 11

ef_7            :: Pitch
ef_7            = flatten $ makeE 11

e_7             :: Pitch
e_7             = makeE 11

es_7            :: Pitch
es_7            = sharpen $ makeE 11

ff_7            :: Pitch
ff_7            = flatten $ makeF 11

f_7             :: Pitch
f_7             = makeF 11

fs_7            :: Pitch
fs_7            = sharpen $ makeF 11

gf_7            :: Pitch
gf_7            = flatten $ makeG 11

g_7             :: Pitch
g_7             = makeG 11

gs_7            :: Pitch
gs_7            = sharpen $ makeG 11

af_7            :: Pitch
af_7            = flatten $ makeA 11

a_7             :: Pitch
a_7             = makeA 11

as_7            :: Pitch
as_7            = sharpen $ makeA 11

bf_7            :: Pitch
bf_7            = flatten $ makeB 11

b_7             :: Pitch
b_7             = makeB 11

bs_7            :: Pitch 
bs_7            = sharpen $ makeB 11


-- Octave 12

cf_8            :: Pitch
cf_8            = flatten $ makeC 12

c_8             :: Pitch
c_8             = makeC 12

cs_8            :: Pitch
cs_8            = sharpen $ makeC 12

df_8            :: Pitch
df_8            = flatten $ makeD 12

d_8             :: Pitch
d_8             = makeD 12

ds_8            :: Pitch
ds_8            = sharpen $ makeD 12

ef_8            :: Pitch
ef_8            = flatten $ makeE 12

e_8             :: Pitch
e_8             = makeE 12

es_8            :: Pitch
es_8            = sharpen $ makeE 12

ff_8            :: Pitch
ff_8            = flatten $ makeF 12

f_8             :: Pitch
f_8             = makeF 12

fs_8            :: Pitch
fs_8            = sharpen $ makeF 12

gf_8            :: Pitch
gf_8            = flatten $ makeG 12

g_8             :: Pitch
g_8             = makeG 12

gs_8            :: Pitch
gs_8            = sharpen $ makeG 12

af_8            :: Pitch
af_8            = flatten $ makeA 12

a_8             :: Pitch
a_8             = makeA 12

as_8            :: Pitch
as_8            = sharpen $ makeA 12

bf_8            :: Pitch
bf_8            = flatten $ makeB 12

b_8             :: Pitch
b_8             = makeB 12

bs_8            :: Pitch 
bs_8            = sharpen $ makeB 12





