{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Cadenza.Internal.CadenzaToMono
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Cadenza syntax to Mono syntax - synthesizing bars 
-- losing beam groups.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Cadenza.Internal.CadenzaToMono
  (
    translateToMono
  ) where


import Payasan.LilyPond.Cadenza.Internal.Syntax

import qualified Payasan.Base.Monophonic.Internal.RecalcBars    as T
import qualified Payasan.Base.Monophonic.Internal.Syntax        as T
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Duration

translateToMono :: Time -> Phrase pch Duration anno -> T.Phrase pch Duration anno
translateToMono ts (Phrase info gs) = 
    let info1 = info { section_meter = TimeSig ts } 
    in T.recalcBars $ phraseT (Phrase info1 gs) 


phraseT ::Phrase pch drn anno -> T.Phrase pch drn anno
phraseT (Phrase info gs)        = T.Phrase info [T.Bar $ concatMap noteGroupT gs]



noteGroupT :: NoteGroup pch drn anno -> [T.NoteGroup pch drn anno]
noteGroupT (Atom e)             = [T.Atom $ elementT e]
noteGroupT (Beamed gs)          = concatMap noteGroupT gs
noteGroupT (Tuplet spec gs)     = [T.Tuplet spec $ concatMap noteGroupT gs]


elementT :: Element pch drn anno -> T.Element pch drn anno
elementT (Note p d a t)         = T.Note p d a t
elementT (Rest d)               = T.Rest d
elementT (Spacer d)             = T.Spacer d
elementT (Skip d)               = T.Skip d
elementT (Punctuation s)        = T.Punctuation s

