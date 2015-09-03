{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Tabular.Output
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output to a Humdrum-like form.
--
-- This is intended debugging and checking purposes, so it is
-- specialized to represent Payasan and is not directly 
-- compatible with Humdrum.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Tabular.Output
  ( 

    lyTabular
    
  ) where

import Payasan.Base.Internal.Tabular.Utils
import qualified Payasan.Base.Internal.LilyPond.Syntax as Ly
import Payasan.Base.Internal.Utils


import Text.PrettyPrint.HughesPJClass                -- package: pretty


lyTabular :: Ly.LyPhrase -> Doc
lyTabular ph = evalTrans (oLyPhrase ph) () 1



-- Generating output is stateful to track bar number

type Mon a = Trans () Int a


nextBar :: Mon Doc
nextBar = do { i <- get; puts (1+); return $ bar i }


oLyPhrase :: Ly.LyPhrase -> Mon Doc
oLyPhrase (Ly.LyPhrase [])       = return empty
oLyPhrase (Ly.LyPhrase (x:xs))   = do { d <- oBar x; step d xs }
  where
    step ac []      = return (ac $+$ endSpine <++> endSpine)
    step ac (b:bs)  = do { d <- oBar b; step (ac $+$ d) bs }


oBar :: Ly.Bar -> Mon Doc
oBar (Ly.Bar _info cs) = ($+$) <$> nextBar <*> pure ( oCtxElementList cs)


oCtxElementList :: [Ly.CtxElement] -> Doc
oCtxElementList xs = vcat $ map oCtxElement xs


oCtxElement :: Ly.CtxElement -> Doc
oCtxElement (Ly.Atom e)         = oElement e
oCtxElement (Ly.Beamed cs)      = oCtxElementList cs
oCtxElement (Ly.Tuplet _ cs)    = oCtxElementList cs

oElement :: Ly.Element -> Doc
oElement (Ly.NoteElem n)        = note n
oElement (Ly.Rest d)            = rest <++> noteLength d 
oElement (Ly.Chord ps d)        = pitches ps <+> noteLength d 
oElement (Ly.Graces xs)         = vcat $ map note xs


note :: Ly.Note -> Doc
note (Ly.Note p d)              = pitch p <++> noteLength d


noteLength :: Ly.NoteLength -> Doc
noteLength (Ly.DrnDefault)      = nullDot
noteLength (Ly.DrnExplicit d)   = duration d

pitch :: Ly.Pitch -> Doc
pitch = pPrint

pitches :: [Ly.Pitch] -> Doc
pitches ps = hcat $ punctuate (char ':') $ map pitch ps