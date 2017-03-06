{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.CadenzaToExternal
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Cadenza syntax to External syntax prior to outputting 
-- LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.Score.Cadenza.Internal.CadenzaToExternal
  (
    fromCadenza
  , transCadenzaToExternal      
  ) where


import Payasan.Score.Cadenza.Internal.Syntax

import qualified Payasan.PSC.Repr.External.Syntax as T

fromCadenza :: Section pch drn anno -> T.Section pch drn anno
fromCadenza = sectionT

-- OLD API - DEPRECATED
transCadenzaToExternal :: Section pch drn anno -> T.Part pch drn anno
transCadenzaToExternal s1 = T.Part [ sectionT s1 ]



sectionT :: Section pch drn anno -> T.Section pch drn anno
sectionT (Section { section_name = name
                  , section_info = info 
                  , section_groups = gs })  = 
    T.Section { T.section_name = name 
              , T.section_info = info 
              , T.section_bars = [T.Bar $ map noteGroupT gs]
              }
  where
    noteGroupT (Atom e)             = T.Atom $ elementT e
    noteGroupT (Beamed xs)          = T.Beamed $ map noteGroupT xs
    noteGroupT (Tuplet spec xs)     = T.Tuplet spec $ map (T.Atom . elementT) xs

    elementT (Note p d a t)         = T.Note p d a t
    elementT (Rest d)               = T.Rest d
    elementT (Spacer d)             = T.Spacer d
    elementT (Skip d)               = T.Skip d
    elementT (Punctuation s)        = T.Punctuation s

