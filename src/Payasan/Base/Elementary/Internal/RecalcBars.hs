{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.RecalcBars
-- Copyright   :  (c) Stephen Tetley 2015-2016
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

module Payasan.Base.Elementary.Internal.RecalcBars
  (
    NoteList(..) 
  , Notes
  , recalcBars
  , viaNoteList
  , onNoteList

  ) where


import Payasan.Base.Elementary.Internal.Syntax

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Duration

import Data.Data


recalcBars :: Part pch Duration anno -> Part pch Duration anno
recalcBars = remake . flatten

type Notes pch anno = [NoteGroup pch Duration anno]


data NoteList pch anno = NoteList
    { notelist_header   :: SectionInfo
    , notelist_notes    :: [NoteGroup pch Duration anno]
    }
  deriving (Data,Eq,Show,Typeable)



-- | Bars maybe be too long or too short upto a time sig (or key)
-- change, so we segment them first.
--
flatten :: StdElemPart2 pch anno -> NoteList pch anno
flatten (Part info bs) = 
    NoteList { notelist_header = info
             , notelist_notes  = concatMap fn bs }
  where
    fn (Bar xs) = xs


viaNoteList :: (SectionInfo -> Notes pch anno -> Notes pch anno) 
            -> StdElemPart2 pch anno
            -> StdElemPart2 pch anno
viaNoteList fn = remake . f2 . flatten
  where
    f2 (NoteList info es) = NoteList info $ fn info es

onNoteList :: (SectionInfo -> Notes pch anno -> ans) 
            -> StdElemPart2 pch anno
            -> ans
onNoteList fn = f2 . flatten
  where
    f2 (NoteList info es) = fn info es


--------------------------------------------------------------------------------
-- Remake - notelist to 

remake :: NoteList pch anno -> StdElemPart2 pch anno
remake (NoteList info es) = 
    Part { part_header = info 
         , part_bars   = fn $ section_meter info }
  where
    fn (Unmetered)  = [Bar es]
    fn (TimeSig t)  = map Bar $ split (barLength t) es

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