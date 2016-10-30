{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.CadenzaToElementary
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Cadenza syntax to Elementary syntax - synthesizing bars 
-- losing beam groups.
--
--------------------------------------------------------------------------------

module Payasan.Score.Cadenza.Internal.CadenzaToElementary
  (
    translateToElem
  ) where


import Payasan.Score.Cadenza.Internal.Syntax

import qualified Payasan.Score.Elementary.Internal.RecalcBars   as T
import qualified Payasan.Score.Elementary.Internal.Syntax       as T
import Payasan.PSC.Base.SyntaxCommon
import Payasan.Base.Duration

translateToElem :: Time -> Part pch Duration anno -> T.Part pch Duration anno
translateToElem ts (Part info gs) = 
    let info1 = info { section_meter = TimeSig ts } 
    in T.recalcBars $ partT (Part info1 gs) 


partT ::Part pch drn anno -> T.Part pch drn anno
partT (Part info gs)    = T.Part info [T.Bar $ concatMap noteGroupT gs]



noteGroupT :: NoteGroup pch drn anno -> [T.NoteGroup pch drn anno]
noteGroupT (Atom e)             = [T.Atom $ elementT e]
noteGroupT (Beamed gs)          = concatMap noteGroupT gs
noteGroupT (Tuplet spec es)     = [T.Tuplet spec $ map elementT es]


elementT :: Element pch drn anno -> T.Element pch drn anno
elementT (Note p d a t)         = T.Note p d a t
elementT (Rest d)               = T.Rest d
elementT (Spacer d)             = T.Spacer d
elementT (Skip d)               = T.Skip d
elementT (Punctuation s)        = T.Punctuation s

