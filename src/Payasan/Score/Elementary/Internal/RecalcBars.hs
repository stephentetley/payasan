{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.RecalcBars
-- Copyright   :  (c) Stephen Tetley 2015-2017
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

module Payasan.Score.Elementary.Internal.RecalcBars
  (
    NoteList(..) 
  , Notes
  , recalcBars
  , viaNoteList
  , onNoteList

  ) where


import Payasan.Score.Elementary.Internal.Syntax

import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Basis
import Payasan.Base.Duration

import Data.Data


recalcBars :: Section pch Duration anno -> Section pch Duration anno
recalcBars = remake . flatten

type Notes pch anno = [NoteGroup pch Duration anno]


data NoteList pch anno = NoteList
    { notelist_name     :: !String
    , notelist_header   :: SectionInfo
    , notelist_notes    :: [NoteGroup pch Duration anno]
    }
  deriving (Data,Eq,Show,Typeable)



-- | Bars maybe be too long or too short upto a time sig (or key)
-- change, so we segment them first.
--
flatten :: Section pch Duration anno -> NoteList pch anno
flatten (Section name info bs) = 
    NoteList { notelist_name = name
             , notelist_header = info
             , notelist_notes  = concatMap fn bs }
  where
    fn (Bar xs) = xs


viaNoteList :: (SectionInfo -> Notes pch anno -> Notes pch anno) 
            -> Section pch Duration anno
            -> Section pch Duration anno
viaNoteList fn = remake . f2 . flatten
  where
    f2 (NoteList name info es) = NoteList name info $ fn info es

onNoteList :: (SectionInfo -> Notes pch anno -> ans) 
            -> Section pch Duration anno
            -> ans
onNoteList fn = f2 . flatten
  where
    f2 (NoteList _ info es) = fn info es


--------------------------------------------------------------------------------
-- Remake - notelist to 

remake :: NoteList pch anno -> Section pch Duration anno
remake (NoteList name info es) = 
    Section { section_name  = name
            , section_info  = info 
            , section_bars  = fn $ section_meter info }
  where
    fn (Unmetered)  = [Bar es]
    fn (Metered t)  = map Bar $ split (timeSigRatDuration t) es

split :: RatDuration 
      -> [NoteGroup pch Duration anno]
      -> [[NoteGroup pch Duration anno]]
split _    [] = []
split dbar cs = let (bar1,rest) = breakBar dbar cs in bar1 : split dbar rest


breakBar :: RatDuration 
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