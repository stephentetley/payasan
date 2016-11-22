{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Repr.ExternalToIRSimpleTile
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

module Payasan.PSC.Repr.ExternalToIRSimpleTile
  ( 
    transExternalToIRSimpleTile
  ) where



import Payasan.PSC.Repr.External.Syntax
import qualified Payasan.PSC.Repr.IRSimpleTile.Syntax as T

import Payasan.Base.Basis (Seconds)

    

transExternalToIRSimpleTile :: Part pch Seconds anno 
                            -> T.Part pch anno
transExternalToIRSimpleTile = partT



partT :: Part pch Seconds anno -> T.Part pch anno
partT (Part bs)                 = 
    error "TODO - ExternalToIRSimpleSimpleTile"
--    T.Part { T.part_bars = map barT bs }


    
barT :: Bar pch Seconds anno -> T.Bar pch anno
barT (Bar gs)                   = 
    T.Bar { T.bar_elems = concatMap noteGroupT gs }
          


-- Assumes we have time-transformed tuplets to their correct 
-- clocktime duration (i.e tuplet spec has already been 
-- interpreted and is now redundant).
--
noteGroupT :: NoteGroup pch Seconds anno 
           -> [T.Element pch anno]
noteGroupT (Atom e)             = elementT e
noteGroupT (Beamed grps)        = concatMap noteGroupT grps
noteGroupT (Tuplet _ grps)      = concatMap noteGroupT grps



-- Translating an element may generate zero (punctuation), 
-- or one (note, rest, chord etc.) event.
--
-- Although Maybe directly captures this, it is more convenient to 
-- use a list for subsequent concatenation.
-- 
elementT :: Element pch Seconds anno -> [T.Element pch anno]
elementT (NoteElem (Note pch drn) anno t)   = [T.Note drn pch anno t]

elementT (Rest drn)                         = [T.Rest drn]

elementT (Spacer drn)                       = [T.Rest drn]

elementT (Skip drn)                         = [T.Rest drn]

elementT (Chord ps drn anno t)              = [T.Chord drn ps anno t]
    
elementT (Graces ns)                        = 
    [T.Graces $ map (\(Note p d) -> (d,p)) ns]
    
elementT (Punctuation {})                   = []



      
