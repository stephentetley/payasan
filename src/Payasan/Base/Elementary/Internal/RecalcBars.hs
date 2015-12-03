{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.RecalcBars
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

module Payasan.Base.Elementary.Internal.RecalcBars
  (
    NoteList(..) 
  , recalcBars
  , viaNoteList

  ) where


import Payasan.Base.Elementary.Internal.Syntax

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Duration

import Data.Data


recalcBars :: Phrase pch Duration anno -> Phrase pch Duration anno
recalcBars = remake . flatten



data NoteList pch anno = NoteList
    { notelist_header   :: SectionInfo
    , notelist_notes    :: [NoteGroup pch Duration anno]
    }
  deriving (Data,Eq,Show,Typeable)



-- | Bars maybe be too long or too short upto a time sig (or key)
-- change, so we segment them first.
--
flatten :: StdElemPhrase2 pch anno -> NoteList pch anno
flatten (Phrase info bs) = 
    NoteList { notelist_header = info
             , notelist_notes  = concatMap fn bs }
  where
    fn (Bar xs) = xs


viaNoteList :: (NoteList pch anno -> NoteList pch anno) 
            -> StdElemPhrase2 pch anno
            -> StdElemPhrase2 pch anno
viaNoteList fn = remake . fn . flatten


--------------------------------------------------------------------------------
-- Remake - notelist to 

remake :: NoteList pch anno -> StdElemPhrase2 pch anno
remake (NoteList info es) = 
    Phrase { phrase_header = info 
           , phrase_bars   = fn $ section_meter info }
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