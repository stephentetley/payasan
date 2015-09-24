{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.Spelling
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

module Payasan.Base.Internal.ABC.Spelling
  (
    SpellingMap(..) 
  , spell    
  , makeSpellingMap

  ) where


import qualified Payasan.Base.Internal.ABC.Syntax as ABC
import Payasan.Base.Internal.ABC.Utils
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Names.Pitch
import Payasan.Base.Pitch


import qualified Data.Map as MAP
import qualified Data.Set as SET

--------------------------------------------------------------------------------
-- Pitch spelling for ABC

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


findAlterations :: Key -> Int
findAlterations (Key n MAJOR)        = MAP.findWithDefault 0 n major_map
findAlterations (Key n MINOR)        = MAP.findWithDefault 0 n minor_map
findAlterations (Key n MIXOLYDIAN)   = MAP.findWithDefault 0 n mix_map
findAlterations (Key n DORIAN)       = MAP.findWithDefault 0 n dor_map
findAlterations (Key n PHRYGIAN)     = MAP.findWithDefault 0 n phr_map
findAlterations (Key n LYDIAN)       = MAP.findWithDefault 0 n lyd_map
findAlterations (Key n LOCRIAN)      = MAP.findWithDefault 0 n loc_map


--------------------------------------------------------------------------------

-- | We need to use the pitch label from Pitch as it doesn\'t
-- confuse pitch letter with upper and lower case.



data SpellingMap = SpellingMap 
    { spelling_map_alterations      :: SET.Set PitchName 
    , spelling_map_naturals         :: SET.Set PitchLetter
    }

-- | Initial translation sets all NATURALS to NO_ACCIDENTAL, we
-- might have to change them to NATURAL for printing.
--
spell :: SpellingMap -> ABC.Pitch -> ABC.Pitch
spell (SpellingMap alts nats) p0@(ABC.Pitch ac l om) = step ac
  where
    step ABC.NO_ACCIDENTAL      = 
        let (l1,_) = toLetterParts l 
        in if l1 `SET.member` nats then ABC.Pitch ABC.NATURAL l om  else p0

    step _                      = 
        let (lbl,_,_) = decomposePitch p0 
        in if lbl `SET.member` alts then ABC.Pitch ABC.NO_ACCIDENTAL l om else p0



makeSpellingMap :: Key -> SpellingMap
makeSpellingMap ks = let i = findAlterations ks in buildSpellings i


-- | Make a spelling map with @n@ accidentals. If @n@ is positive
-- the accidentals will be sharps, if @n@ s negative the 
-- accidentals will be flats.
--
buildSpellings :: Int -> SpellingMap
buildSpellings n 
    | abs n > 7 = error "SpellingMap.makeSpellingMap - more sharps/flats than notes."
    | n == 0    = SpellingMap SET.empty SET.empty
    | n >  0    = build $ nsharps n
    | otherwise = build $ nflats (abs n)          
  where
    build lbls = SpellingMap 
                   { spelling_map_alterations = SET.fromList $ lbls
                   , spelling_map_naturals    = SET.fromList $ map root lbls
                   }
    root (PitchName l _)   = l





nsharps :: Int -> [PitchName]
nsharps n = map mksharp $ take n [F, C, G, D, A, E, B] 
  where
    mksharp l = PitchName l SHARP

nflats :: Int -> [PitchName]
nflats n = map mkflat $ take n [B, E, A, D, G, C, F] 
  where
    mkflat l = PitchName l FLAT

