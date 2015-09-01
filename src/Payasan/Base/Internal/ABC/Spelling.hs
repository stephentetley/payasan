{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  SymNotelist.Internal.ABCSpelling
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

module SymNotelist.Internal.ABCSpelling
  (
    SpellingMap(..) 
  , spell    
  , makeSpellingMap

  ) where

import qualified SymNotelist.Internal.ABCSyntax as ABC
import SymNotelist.Internal.ABCUtils
import SymNotelist.Internal.CommonSyntax (KeySig(..), Mode(..))
import SymNotelist.NoteNames
import SymNotelist.Pitch

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

loc_map :: MAP.Map NoteLabel Int
loc_map = MAP.fromList $ 
    [ (bsharp,7), (esharp,6), (asharp,5), (dsharp,4)
    , (gsharp,3), (csharp,2), (fnat,1)
    , (bnat,0)
    , (enat,-1), (anat,-2), (dnat,-3), (gnat,-4)
    , (cnat,-5), (fnat,-6), (bflat,-7)
    ]

lyd_map :: MAP.Map NoteLabel Int
lyd_map = MAP.fromList $ 
    [ (fsharp,7), (bnat,6), (enat,5), (anat,4)
    , (dnat,3), (gnat,2), (cnat,1)
    , (fnat,0)
    , (bflat,-1), (eflat,-2), (aflat,-3), (dflat,-4)
    , (gflat,-5), (cflat,-6), (fflat,-7)
    ]

phr_map :: MAP.Map NoteLabel Int
phr_map = MAP.fromList $ 
    [ (esharp,7), (asharp,6), (dsharp,5), (gsharp,4)
    , (csharp,3), (fsharp,2), (bnat,1)
    , (enat,0)
    , (anat,-1), (dnat,-2), (gnat,-3), (cnat,-4)
    , (fnat,-5), (bflat,-6), (eflat,-7)
    ]

dor_map :: MAP.Map NoteLabel Int
dor_map = MAP.fromList $ 
    [ (dsharp,7), (gsharp,6), (csharp,5), (fsharp,4)
    , (bnat,3), (enat,2), (anat,1)
    , (dnat,0)
    , (gnat,-1), (cnat,-2), (fnat,-3), (bflat,-4)
    , (eflat,-5), (aflat,-6), (dflat,-7)
    ]

mix_map :: MAP.Map NoteLabel Int
mix_map = MAP.fromList $ 
    [ (gsharp,7), (csharp,6), (fsharp,5), (bnat,4)
    , (enat,3), (anat,2), (dnat,1)
    , (gnat,0)
    , (cnat,-1), (fnat,-2), (bflat,-3), (eflat,-4)
    , (aflat,-5), (dflat,-6), (gflat,-7)
    ]

minor_map :: MAP.Map NoteLabel Int
minor_map = MAP.fromList $ 
    [ (asharp,7), (dsharp,6), (gsharp,5), (csharp,4)
    , (fsharp,3), (bnat,2), (enat,1)
    , (enat,0)
    , (dnat,-1), (gnat,-2), (cnat,-3), (fnat,-4)
    , (bflat,-5), (eflat,-6), (aflat,-7)
    ]


major_map :: MAP.Map NoteLabel Int
major_map = MAP.fromList $ 
    [ (csharp,7), (fsharp,6), (bnat,5), (enat,4)
    , (anat,3), (dnat,2), (gnat,1)
    , (cnat,0)
    , (fnat,-1), (bflat,-2), (eflat,-3), (aflat,-4)
    , (dflat,-5), (gflat,-6), (cflat,-7)
    ]


findAlterations :: KeySig -> Int
findAlterations (KeySig n MAJOR)        = MAP.findWithDefault 0 n major_map
findAlterations (KeySig n MINOR)        = MAP.findWithDefault 0 n minor_map
findAlterations (KeySig n MIXOLYDIAN)   = MAP.findWithDefault 0 n mix_map
findAlterations (KeySig n DORIAN)       = MAP.findWithDefault 0 n dor_map
findAlterations (KeySig n PHRYGIAN)     = MAP.findWithDefault 0 n phr_map
findAlterations (KeySig n LYDIAN)       = MAP.findWithDefault 0 n lyd_map
findAlterations (KeySig n LOCRIAN)      = MAP.findWithDefault 0 n loc_map


--------------------------------------------------------------------------------

-- | We need to use the pitch label from Pitch as it doesn\'t
-- confuse pitch letter with upper and lower case.



data SpellingMap = SpellingMap 
    { spelling_map_alterations      :: SET.Set NoteLabel 
    , spelling_map_naturals         :: SET.Set PitchLetter
    }

-- | Initial translation sets all NATURALS to NO_ACCIDENTAL, we
-- might  have to change them to NATURAL for printing.
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



makeSpellingMap :: KeySig -> SpellingMap
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
    root (NoteLabel l _)   = l





nsharps :: Int -> [NoteLabel]
nsharps n = map mksharp $ take n [F, C, G, D, A, E, B] 
  where
    mksharp l = NoteLabel l SHARP

nflats :: Int -> [NoteLabel]
nflats n = map mkflat $ take n [B, E, A, D, G, C, F] 
  where
    mkflat l = NoteLabel l FLAT

