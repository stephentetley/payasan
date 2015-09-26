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

   -- * Pitch Names
    c_flat, c_nat, c_sharp
  , d_flat, d_nat, d_sharp
  , e_flat, e_nat, e_sharp
  , f_flat, f_nat, f_sharp
  , g_flat, g_nat, g_sharp
  , a_flat, a_nat, a_sharp
  , b_flat, b_nat, b_sharp

  -- * Pitches with octave
  
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

import Payasan.Base.Pitch


--------------------------------------------------------------------------------
-- Pitch names

c_flat          :: PitchName
c_flat          = PitchName C FLAT

-- c_nat is a re-export from Payasan.Base.Pitch 

c_sharp         :: PitchName
c_sharp         = PitchName C SHARP

-- D

d_flat          :: PitchName
d_flat          = PitchName D FLAT

d_nat           :: PitchName
d_nat           = PitchName D NAT

d_sharp         :: PitchName
d_sharp         = PitchName D SHARP

-- E

e_flat          :: PitchName
e_flat          = PitchName E FLAT

e_nat           :: PitchName
e_nat           = PitchName E NAT

e_sharp         :: PitchName
e_sharp         = PitchName E SHARP

-- F 

f_flat          :: PitchName
f_flat          = PitchName F FLAT

f_nat           :: PitchName
f_nat           = PitchName F NAT

f_sharp         :: PitchName
f_sharp         = PitchName F SHARP

-- G 

g_flat          :: PitchName
g_flat          = PitchName G FLAT

g_nat           :: PitchName
g_nat           = PitchName G NAT

g_sharp         :: PitchName
g_sharp         = PitchName G SHARP

-- A

a_flat          :: PitchName
a_flat          = PitchName A FLAT

a_nat           :: PitchName
a_nat           = PitchName A NAT

a_sharp         :: PitchName
a_sharp         = PitchName A SHARP

-- B

b_flat          :: PitchName
b_flat          = PitchName B FLAT

b_nat           :: PitchName
b_nat           = PitchName B NAT

b_sharp         :: PitchName
b_sharp         = PitchName B SHARP


--------------------------------------------------------------------------------
-- Pitches with octave


makeC           :: Int -> Pitch
makeC ove       = Pitch c_nat ove

makeD           :: Int -> Pitch
makeD ove       = Pitch d_nat ove

makeE            :: Int -> Pitch
makeE ove       = Pitch e_nat ove

makeF           :: Int -> Pitch
makeF ove       = Pitch f_nat ove

makeG           :: Int -> Pitch
makeG ove       = Pitch g_nat ove

makeA           :: Int -> Pitch
makeA ove       = Pitch a_nat ove

makeB           :: Int -> Pitch
makeB ove       = Pitch b_nat ove






-- Octave 1

cf_1            :: Pitch
cf_1            = flatOf $ makeC 1

c_1             :: Pitch
c_1             = makeC 1

cs_1            :: Pitch
cs_1            = sharpOf $ makeC 1

df_1            :: Pitch
df_1            = flatOf $ makeD 1

d_1             :: Pitch
d_1             = makeD 1

ds_1            :: Pitch
ds_1            = sharpOf $ makeD 1

ef_1            :: Pitch
ef_1            = flatOf $ makeE 1

e_1             :: Pitch
e_1             = makeE 1

es_1            :: Pitch
es_1            = sharpOf $ makeE 1

ff_1            :: Pitch
ff_1            = flatOf $ makeF 1

f_1             :: Pitch
f_1             = makeF 1

fs_1            :: Pitch
fs_1            = sharpOf $ makeF 1

gf_1            :: Pitch
gf_1            = flatOf $ makeG 1

g_1             :: Pitch
g_1             = makeG 1

gs_1            :: Pitch
gs_1            = sharpOf $ makeG 1

af_1            :: Pitch
af_1            = flatOf $ makeA 1

a_1             :: Pitch
a_1             = makeA 1

as_1            :: Pitch
as_1            = sharpOf $ makeA 1

bf_1            :: Pitch
bf_1            = flatOf $ makeB 1

b_1             :: Pitch
b_1             = makeB 1

bs_1            :: Pitch 
bs_1            = sharpOf $ makeB 1


-- Octave 2

cf_2            :: Pitch
cf_2            = flatOf $ makeC 2

c_2             :: Pitch
c_2             = makeC 2

cs_2            :: Pitch
cs_2            = sharpOf $ makeC 2

df_2            :: Pitch
df_2            = flatOf $ makeD 2

d_2             :: Pitch
d_2             = makeD 2

ds_2            :: Pitch
ds_2            = sharpOf $ makeD 2

ef_2            :: Pitch
ef_2            = flatOf $ makeE 2

e_2             :: Pitch
e_2             = makeE 2

es_2            :: Pitch
es_2            = sharpOf $ makeE 2

ff_2            :: Pitch
ff_2            = flatOf $ makeF 2

f_2             :: Pitch
f_2             = makeF 2

fs_2            :: Pitch
fs_2            = sharpOf $ makeF 2

gf_2            :: Pitch
gf_2            = flatOf $ makeG 2

g_2             :: Pitch
g_2             = makeG 2

gs_2            :: Pitch
gs_2            = sharpOf $ makeG 2

af_2            :: Pitch
af_2            = flatOf $ makeA 2

a_2             :: Pitch
a_2             = makeA 2

as_2            :: Pitch
as_2            = sharpOf $ makeA 2

bf_2            :: Pitch
bf_2            = flatOf $ makeB 2

b_2             :: Pitch
b_2             = makeB 2

bs_2            :: Pitch 
bs_2            = sharpOf $ makeB 2



-- Octave 3

cf_3            :: Pitch
cf_3            = flatOf $ makeC 3

c_3             :: Pitch
c_3             = makeC 3

cs_3            :: Pitch
cs_3            = sharpOf $ makeC 3

df_3            :: Pitch
df_3            = flatOf $ makeD 3

d_3             :: Pitch
d_3             = makeD 3

ds_3            :: Pitch
ds_3            = sharpOf $ makeD 3

ef_3            :: Pitch
ef_3            = flatOf $ makeE 3

e_3             :: Pitch
e_3             = makeE 3

es_3            :: Pitch
es_3            = sharpOf $ makeE 3

ff_3            :: Pitch
ff_3            = flatOf $ makeF 3

f_3             :: Pitch
f_3             = makeF 3

fs_3            :: Pitch
fs_3            = sharpOf $ makeF 3

gf_3            :: Pitch
gf_3            = flatOf $ makeG 3

g_3             :: Pitch
g_3             = makeG 3

gs_3            :: Pitch
gs_3            = sharpOf $ makeG 3

af_3            :: Pitch
af_3            = flatOf $ makeA 3

a_3             :: Pitch
a_3             = makeA 3

as_3            :: Pitch
as_3            = sharpOf $ makeA 3

bf_3            :: Pitch
bf_3            = flatOf $ makeB 3

b_3             :: Pitch
b_3             = makeB 3

bs_3            :: Pitch 
bs_3            = sharpOf $ makeB 3


-- Octave 4

cf_4            :: Pitch
cf_4            = flatOf $ makeC 4

c_4             :: Pitch
c_4             = makeC 4

cs_4            :: Pitch
cs_4            = sharpOf $ makeC 4

df_4            :: Pitch
df_4            = flatOf $ makeD 4

d_4             :: Pitch
d_4             = makeD 4

ds_4            :: Pitch
ds_4            = sharpOf $ makeD 4

ef_4            :: Pitch
ef_4            = flatOf $ makeE 4

e_4             :: Pitch
e_4             = makeE 4

es_4            :: Pitch
es_4            = sharpOf $ makeE 4

ff_4            :: Pitch
ff_4            = flatOf $ makeF 4

f_4             :: Pitch
f_4             = makeF 4

fs_4            :: Pitch
fs_4            = sharpOf $ makeF 4

gf_4            :: Pitch
gf_4            = flatOf $ makeG 4

g_4             :: Pitch
g_4             = makeG 4

gs_4            :: Pitch
gs_4            = sharpOf $ makeG 4

af_4            :: Pitch
af_4            = flatOf $ makeA 4

a_4             :: Pitch
a_4             = makeA 4

as_4            :: Pitch
as_4            = sharpOf $ makeA 4

bf_4            :: Pitch
bf_4            = flatOf $ makeB 4

b_4             :: Pitch
b_4             = makeB 4

bs_4            :: Pitch 
bs_4            = sharpOf $ makeB 4


-- Octave 5

cf_5            :: Pitch
cf_5            = flatOf $ makeC 5

c_5             :: Pitch
c_5             = makeC 5

cs_5            :: Pitch
cs_5            = sharpOf $ makeC 5

df_5            :: Pitch
df_5            = flatOf $ makeD 5

d_5             :: Pitch
d_5             = makeD 5

ds_5            :: Pitch
ds_5            = sharpOf $ makeD 5

ef_5            :: Pitch
ef_5            = flatOf $ makeE 5

e_5             :: Pitch
e_5             = makeE 5

es_5            :: Pitch
es_5            = sharpOf $ makeE 5

ff_5            :: Pitch
ff_5            = flatOf $ makeF 5

f_5             :: Pitch
f_5             = makeF 5

fs_5            :: Pitch
fs_5            = sharpOf $ makeF 5

gf_5            :: Pitch
gf_5            = flatOf $ makeG 5

g_5             :: Pitch
g_5             = makeG 5

gs_5            :: Pitch
gs_5            = sharpOf $ makeG 5

af_5            :: Pitch
af_5            = flatOf $ makeA 5

a_5             :: Pitch
a_5             = makeA 5

as_5            :: Pitch
as_5            = sharpOf $ makeA 5

bf_5            :: Pitch
bf_5            = flatOf $ makeB 5

b_5             :: Pitch
b_5             = makeB 5

bs_5            :: Pitch 
bs_5            = sharpOf $ makeB 5


-- Octave 6

cf_6            :: Pitch
cf_6            = flatOf $ makeC 6

c_6             :: Pitch
c_6             = makeC 6

cs_6            :: Pitch
cs_6            = sharpOf $ makeC 6

df_6            :: Pitch
df_6            = flatOf $ makeD 6

d_6             :: Pitch
d_6             = makeD 6

ds_6            :: Pitch
ds_6            = sharpOf $ makeD 6

ef_6            :: Pitch
ef_6            = flatOf $ makeE 6

e_6             :: Pitch
e_6             = makeE 6

es_6            :: Pitch
es_6            = sharpOf $ makeE 6

ff_6            :: Pitch
ff_6            = flatOf $ makeF 6

f_6             :: Pitch
f_6             = makeF 6

fs_6            :: Pitch
fs_6            = sharpOf $ makeF 6

gf_6            :: Pitch
gf_6            = flatOf $ makeG 6

g_6             :: Pitch
g_6             = makeG 6

gs_6            :: Pitch
gs_6            = sharpOf $ makeG 6

af_6            :: Pitch
af_6            = flatOf $ makeA 6

a_6             :: Pitch
a_6             = makeA 6

as_6            :: Pitch
as_6            = sharpOf $ makeA 6

bf_6            :: Pitch
bf_6            = flatOf $ makeB 6

b_6             :: Pitch
b_6             = makeB 6

bs_6            :: Pitch 
bs_6            = sharpOf $ makeB 6


-- Octave 7

cf_7            :: Pitch
cf_7            = flatOf $ makeC 7

c_7             :: Pitch
c_7             = makeC 7

cs_7            :: Pitch
cs_7            = sharpOf $ makeC 7

df_7            :: Pitch
df_7            = flatOf $ makeD 7

d_7             :: Pitch
d_7             = makeD 7

ds_7            :: Pitch
ds_7            = sharpOf $ makeD 7

ef_7            :: Pitch
ef_7            = flatOf $ makeE 7

e_7             :: Pitch
e_7             = makeE 7

es_7            :: Pitch
es_7            = sharpOf $ makeE 7

ff_7            :: Pitch
ff_7            = flatOf $ makeF 7

f_7             :: Pitch
f_7             = makeF 7

fs_7            :: Pitch
fs_7            = sharpOf $ makeF 7

gf_7            :: Pitch
gf_7            = flatOf $ makeG 7

g_7             :: Pitch
g_7             = makeG 7

gs_7            :: Pitch
gs_7            = sharpOf $ makeG 7

af_7            :: Pitch
af_7            = flatOf $ makeA 7

a_7             :: Pitch
a_7             = makeA 7

as_7            :: Pitch
as_7            = sharpOf $ makeA 7

bf_7            :: Pitch
bf_7            = flatOf $ makeB 7

b_7             :: Pitch
b_7             = makeB 7

bs_7            :: Pitch 
bs_7            = sharpOf $ makeB 7


-- Octave 8

cf_8            :: Pitch
cf_8            = flatOf $ makeC 8

c_8             :: Pitch
c_8             = makeC 8

cs_8            :: Pitch
cs_8            = sharpOf $ makeC 8

df_8            :: Pitch
df_8            = flatOf $ makeD 8

d_8             :: Pitch
d_8             = makeD 8

ds_8            :: Pitch
ds_8            = sharpOf $ makeD 8

ef_8            :: Pitch
ef_8            = flatOf $ makeE 8

e_8             :: Pitch
e_8             = makeE 8

es_8            :: Pitch
es_8            = sharpOf $ makeE 8

ff_8            :: Pitch
ff_8            = flatOf $ makeF 8

f_8             :: Pitch
f_8             = makeF 8

fs_8            :: Pitch
fs_8            = sharpOf $ makeF 8

gf_8            :: Pitch
gf_8            = flatOf $ makeG 8

g_8             :: Pitch
g_8             = makeG 8

gs_8            :: Pitch
gs_8            = sharpOf $ makeG 8

af_8            :: Pitch
af_8            = flatOf $ makeA 8

a_8             :: Pitch
a_8             = makeA 8

as_8            :: Pitch
as_8            = sharpOf $ makeA 8

bf_8            :: Pitch
bf_8            = flatOf $ makeB 8

b_8             :: Pitch
b_8             = makeB 8

bs_8            :: Pitch 
bs_8            = sharpOf $ makeB 8





