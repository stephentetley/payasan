{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Scale
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Scale building for pitch spelling, etc.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Scale
  ( 

    Scale
  , fromScale
  , buildScale

  , isScaleTone
  , findAlteration
  , alterationForShorthand
  , alterationForLonghand
  

  ) where


import Payasan.Base.Internal.SyntaxCommon
import Payasan.Base.Pitch
import Payasan.Base.Names.Pitch


import Data.Data
import qualified Data.List as LIST
import qualified Data.Map as MAP


-- perhaps Scale could also include name::String for debugging

newtype Scale = Scale { fromScale :: [PitchName] }
  deriving (Data,Eq,Show,Typeable)



buildScale :: Key -> Scale
buildScale key@(Key root _) = 
   let i = countAlterations key
   in Scale $ orderPitchNames root $ pitchNamesUnordered i


orderPitchNames :: PitchName -> [PitchName] -> [PitchName]
orderPitchNames first xs = afters ++ befores
  where
    ys = LIST.sortBy (\x y -> pitch_letter x `compare` pitch_letter y) xs
    (befores,afters)  = LIST.span (\x -> x /= first) ys



pitchNamesUnordered :: Int -> [PitchName]
pitchNamesUnordered n =
    map mk1 [C,D,E,F,G,A,B]
  where
    accidentals = if n >= 0 then nsharps n else nflats (abs n)
    mk1 pl      = case findFromRoot1 pl accidentals of
                    Just name -> name
                    Nothing -> PitchName pl NAT 

findFromRoot1 :: PitchLetter -> [PitchName] -> Maybe PitchName
findFromRoot1 pl xs = LIST.find (\a -> pl == pitch_letter a) xs




isScaleTone :: Pitch -> Scale -> Bool
isScaleTone (Pitch lbl _) sc = let xs = fromScale sc in lbl `elem` xs

findAlteration :: PitchLetter -> Scale -> Maybe Alteration
findAlteration pl sc = 
    fmap pitch_alteration $ LIST.find (\a -> pl == pitch_letter a) (fromScale sc)


-- | Lookup a /natural/ note to see if it is altered in the scale.
-- 
alterationForShorthand :: Scale -> Pitch -> Maybe Alteration
alterationForShorthand sc p@(Pitch (PitchName ltr _) _)
    | isAltered p = Nothing
    | otherwise   = findAlteration ltr sc >>= \alt -> 
                    if alt == NAT then Nothing else Just alt


alterationForLonghand :: Scale -> Pitch -> Maybe Alteration
alterationForLonghand sc p
    | isScaleTone p sc && isAltered p = Just NAT
    | otherwise                       = Nothing



nsharps :: Int -> [PitchName]
nsharps n = map mksharp $ take n [F, C, G, D, A, E, B] 
  where
    mksharp l = PitchName l SHARP

nflats :: Int -> [PitchName]
nflats n = map mkflat $ take n [B, E, A, D, G, C, F] 
  where
    mkflat l = PitchName l FLAT




--------------------------------------------------------------------------------
-- Scales

{-
-- 7 sharps:   C#      A#m      G#Mix   D#Dor   E#Phr   F#Lyd   B#Loc
-- 6 sharps:   F#      D#m      C#Mix   G#Dor   A#Phr   BLyd    E#Loc
-- 5 sharps:   B       G#m      F#Mix   C#Dor   D#Phr   ELyd    A#Loc
-- 4 sharps:   E       C#m      BMix    F#Dor   G#Phr   ALyd    D#Loc
-- 3 sharps:   A       F#m      EMix    BDor    C#Phr   DLyd    G#Loc
-- 2 sharps:   D       Bm       AMix    EDor    F#Phr   GLyd    C#Loc
-- 1 sharp :   G       Em       DMix    ADor    BPhr    CLyd    F#Loc
-- 0 sharps:   C       Am       GMix    DDor    EPhr    FLyd    BLoc
-- 1 flat  :   F       Dm       CMix    GDor    APhr    BbLyd   ELoc
-- 2 flats :   Bb      Gm       FMix    CDor    DPhr    EbLyd   ALoc
-- 3 flats :   Eb      Cm       BbMix   FDor    GPhr    AbLyd   DLoc
-- 4 flats :   Ab      Fm       EbMix   BbDor   CPhr    DbLyd   GLoc
-- 5 flats :   Db      Bbm      AbMix   EbDor   FPhr    GbLyd   CLoc
-- 6 flats :   Gb      Ebm      DbMix   AbDor   BbPhr   CbLyd   FLoc
-- 7 flats :   Cb      Abm      GbMix   DbDor   EbPhr   FbLyd   BbLoc
-}

loc_map :: MAP.Map PitchName Int
loc_map = MAP.fromList $ 
    [ (b_sharp,7), (e_sharp,6), (a_sharp,5), (d_sharp,4)
    , (g_sharp,3), (c_sharp,2), (f_nat,1)
    , (b_nat,0)
    , (e_nat,-1), (a_nat,-2), (d_nat,-3), (g_nat,-4)
    , (c_nat,-5), (f_nat,-6), (b_flat,-7)
    ]

lyd_map :: MAP.Map PitchName Int
lyd_map = MAP.fromList $ 
    [ (f_sharp,7), (b_nat,6), (e_nat,5), (a_nat,4)
    , (d_nat,3), (g_nat,2), (c_nat,1)
    , (f_nat,0)
    , (b_flat,-1), (e_flat,-2), (a_flat,-3), (d_flat,-4)
    , (g_flat,-5), (c_flat,-6), (f_flat,-7)
    ]

phr_map :: MAP.Map PitchName Int
phr_map = MAP.fromList $ 
    [ (e_sharp,7), (a_sharp,6), (d_sharp,5), (g_sharp,4)
    , (c_sharp,3), (f_sharp,2), (b_nat,1)
    , (e_nat,0)
    , (a_nat,-1), (d_nat,-2), (g_nat,-3), (c_nat,-4)
    , (f_nat,-5), (b_flat,-6), (e_flat,-7)
    ]

dor_map :: MAP.Map PitchName Int
dor_map = MAP.fromList $ 
    [ (d_sharp,7), (g_sharp,6), (c_sharp,5), (f_sharp,4)
    , (b_nat,3), (e_nat,2), (a_nat,1)
    , (d_nat,0)
    , (g_nat,-1), (c_nat,-2), (f_nat,-3), (b_flat,-4)
    , (e_flat,-5), (a_flat,-6), (d_flat,-7)
    ]

mix_map :: MAP.Map PitchName Int
mix_map = MAP.fromList $ 
    [ (g_sharp,7), (c_sharp,6), (f_sharp,5), (b_nat,4)
    , (e_nat,3), (a_nat,2), (d_nat,1)
    , (g_nat,0)
    , (c_nat,-1), (f_nat,-2), (b_flat,-3), (e_flat,-4)
    , (a_flat,-5), (d_flat,-6), (g_flat,-7)
    ]

minor_map :: MAP.Map PitchName Int
minor_map = MAP.fromList $ 
    [ (a_sharp,7), (d_sharp,6), (g_sharp,5), (c_sharp,4)
    , (f_sharp,3), (b_nat,2), (e_nat,1)
    , (e_nat,0)
    , (d_nat,-1), (g_nat,-2), (c_nat,-3), (f_nat,-4)
    , (b_flat,-5), (e_flat,-6), (a_flat,-7)
    ]


major_map :: MAP.Map PitchName Int
major_map = MAP.fromList $ 
    [ (c_sharp,7), (f_sharp,6), (b_nat,5), (e_nat,4)
    , (a_nat,3), (d_nat,2), (g_nat,1)
    , (c_nat,0)
    , (f_nat,-1), (b_flat,-2), (e_flat,-3), (a_flat,-4)
    , (d_flat,-5), (g_flat,-6), (c_flat,-7)
    ]


countAlterations :: Key -> Int
countAlterations (Key n MAJOR)        = MAP.findWithDefault 0 n major_map
countAlterations (Key n MINOR)        = MAP.findWithDefault 0 n minor_map
countAlterations (Key n MIXOLYDIAN)   = MAP.findWithDefault 0 n mix_map
countAlterations (Key n DORIAN)       = MAP.findWithDefault 0 n dor_map
countAlterations (Key n PHRYGIAN)     = MAP.findWithDefault 0 n phr_map
countAlterations (Key n LYDIAN)       = MAP.findWithDefault 0 n lyd_map
countAlterations (Key n LOCRIAN)      = MAP.findWithDefault 0 n loc_map


