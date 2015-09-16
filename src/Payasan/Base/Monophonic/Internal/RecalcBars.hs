{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.RecalcBars
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Recalculate bars according to time signature.
--
-- Necessary after augmentation or diminution.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.RecalcBars
  (
    recalcBars
  ) where


import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Duration


recalcBars :: Phrase pch Duration -> Phrase pch Duration
recalcBars (Phrase { phrase_bars = bs }) = 
    let ss = segment bs in Phrase $ remake ss



data Segment pch = Segment 
    { _segment_header   :: LocalRenderInfo
    , _segment_notes    :: [NoteGroup pch Duration]
    }



-- | Bars maybe be too long or too short upto a time sig (or key)
-- change, so we segment them first.
--
segment :: [Bar pch Duration] -> [Segment pch]
segment []     = []
segment (b:bs) = step (bar_header b) [] [] b bs
  where
    step info ac acc x [] 
        | info == bar_header x  = acc ++ [Segment info (ac ++ bar_elements x)]
        | otherwise             = let penul = Segment info ac
                                      slast = Segment (bar_header x) (bar_elements x)
                                  in acc ++ [ penul, slast ]
                                        
    step info ac acc x (y:ys)
        | info == bar_header x  = step info (ac ++ bar_elements x) acc y ys
        | otherwise             = let acc1 = if null ac then acc else acc ++ [Segment info ac]
                                  in step (bar_header x) (bar_elements x) acc1 y ys



remake :: [Segment pch] -> [Bar pch Duration]
remake = concatMap remake1

remake1 :: Segment pch -> [Bar pch Duration]
remake1 (Segment info es) = 
    map (Bar info) $ split (barLength $ local_meter info) es

split :: RDuration -> [NoteGroup pch Duration] -> [[NoteGroup pch Duration]]
split _    [] = []
split dbar cs = let (bar1,rest) = breakBar dbar cs in bar1 : split dbar rest


breakBar :: RDuration 
         -> [NoteGroup pch Duration] 
         -> ([NoteGroup pch Duration], [NoteGroup pch Duration])
breakBar _    [] = ([],[])
breakBar dbar cs = step 0 [] cs
  where
    step _ ac []                        = (ac,[])
    
    -- Special case always generate non-empty bars even if note 
    -- length of of a singleton is too big.
    step _ [] (x:xs)  
         | sizeNoteGroup x >= dbar     = ([x],xs)

    step n ac xs@(x:_)
         | n + sizeNoteGroup x > dbar  = (ac,xs)

    step n ac (x:xs)                    = step (n+sizeNoteGroup x) (ac++[x]) xs