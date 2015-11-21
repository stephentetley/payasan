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
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Duration


recalcBars :: Phrase pch Duration anno -> Phrase pch Duration anno
recalcBars (Phrase { phrase_header = info, phrase_bars = bs }) = 
    let s = segment info bs in Phrase { phrase_header = info
                                      , phrase_bars   = remake s
                                      }



data Segment pch anno = Segment 
    { segment_header   :: LocalContextInfo
    , segment_notes    :: [NoteGroup pch Duration anno]
    }



-- | Bars maybe be too long or too short upto a time sig (or key)
-- change, so we segment them first.
--
segment :: LocalContextInfo -> [Bar pch Duration anno] -> Segment pch anno
segment info bs = Segment { segment_header = info
                          , segment_notes  = concatMap fn bs }
  where
    fn (Bar xs) = xs


remake :: Segment pch anno -> [Bar pch Duration anno]
remake (Segment info es) = case local_meter info of
    Unmetered -> [Bar es]
    TimeSig t -> map Bar $ split (barLength t) es

split :: RDuration 
      -> [NoteGroup pch Duration anno]
      -> [[NoteGroup pch Duration anno]]
split _    [] = []
split dbar cs = let (bar1,rest) = breakBar dbar cs in bar1 : split dbar rest


breakBar :: RDuration 
         -> [NoteGroup pch Duration anno] 
         -> ([NoteGroup pch Duration anno], [NoteGroup pch Duration anno])
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