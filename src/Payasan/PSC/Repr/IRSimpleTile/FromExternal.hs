{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.IRSimpleTile.FromExternal
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translate External To IRSimpleTile.
--
-- External must have be already translated to have Seconds 
-- (duration). 
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Repr.IRSimpleTile.FromExternal
  ( 
    fromExternal
  ) where



import Payasan.PSC.Repr.External.Syntax
import qualified Payasan.PSC.Repr.IRSimpleTile.Syntax as T
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Basis (Seconds)

import Data.List (mapAccumL)
    

fromExternal :: Part pch Seconds anno -> T.Part pch anno
fromExternal = partT



partT :: Part pch Seconds anno -> T.Part pch anno
partT (Part ss)                 =  
    T.Part { T.part_sections = snd $ mapAccumL sectionA 0 ss } 


-- Ties cannot span sections, so we seed the tie state here.
-- 
-- Essentially this transformation turns ties into suffixes
-- rather than prefixes.
-- 
sectionA :: Seconds -> Section pch Seconds anno -> (Seconds, T.Section pch anno)
sectionA ot (Section name _ bs)    = (ot+drn,section1)
  where 
     bars     = snd $ mapAccumL barA (Acc NO_TIE ot) bs 
     drn      = sum $ map T.barDuration bars
     section1 = T.Section { T.section_name  = name
                          , T.section_onset = ot
                          , T.section_bars  = bars
                          }

data Acc = Acc !Tie !Seconds


barA :: Acc -> Bar pch Seconds anno -> (Acc, T.Bar pch anno)
barA (Acc t ot) (Bar gs)            = (Acc t1 (ot + drn), bar1)
  where
    (t1,ess)     = mapAccumL noteGroupA t gs
    elems        = concat ess
    drn          = sum $ map T.elementDuration elems
    bar1         = T.Bar { T.bar_onset = ot
                         , T.bar_elems = elems
                         }



-- Assumes we have time-transformed tuplets to their correct 
-- clocktime duration (i.e tuplet spec has already been 
-- interpreted and is now redundant).
--
noteGroupA :: Tie 
           -> NoteGroup pch Seconds anno 
           -> (Tie, [T.Element pch anno])
noteGroupA t (Atom e)             = elementA t e

noteGroupA t (Beamed grps)        = 
    let (t1,xss) = mapAccumL noteGroupA t grps in (t1, concat xss)

noteGroupA t (Tuplet _ grps)      = 
    let (t1,xss) = mapAccumL noteGroupA t grps in (t1, concat xss)





-- Translating an element may generate zero (punctuation), 
-- or one (note, rest, chord etc.) event.
--
-- Although Maybe directly captures this, it is more convenient to 
-- use a list for subsequent concatenation.
-- 
elementA :: Tie -> Element pch Seconds anno -> (Tie, [T.Element pch anno])
elementA t (NoteElem (Note pch drn) ann t1)   = 
    case t of 
      NO_TIE -> (t1, [T.Note drn pch ann])
      TIE -> (t1, [T.TiedCont drn])

elementA _ (Rest drn)                         = (NO_TIE, [T.Rest drn])

elementA _ (Spacer drn)                       = (NO_TIE, [T.Rest drn])

elementA _ (Skip drn)                         = (NO_TIE, [T.Rest drn])

elementA t (Chord ps drn ann t1)              = 
    case t of 
      NO_TIE -> (t1, [T.Chord drn ps ann])
      TIE -> (t1, [T.TiedCont drn])
    
elementA _ (Graces ns)                        = 
    (NO_TIE, [T.Graces $ map (\(Note p d) -> (d,p)) ns])
    
elementA _ (Punctuation {})                   = (NO_TIE, [])



      
