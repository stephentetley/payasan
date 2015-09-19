{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.Transform
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Musical transfomations (augmentation, diminution etc.)
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.Transform
  (
    augment
  , diminute
  ) where


import Payasan.Base.Monophonic.Internal.RecalcBars
import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Monophonic.Internal.Traversals

import Payasan.Base.Duration

augment :: Phrase pch Duration anno -> Phrase pch Duration anno
augment = recalcBars . mapDrn doubleDuration

diminute :: Phrase pch Duration anno -> Phrase pch Duration anno
diminute = recalcBars . mapDrn halveDuration