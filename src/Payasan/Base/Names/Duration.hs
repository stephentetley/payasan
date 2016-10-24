{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Names.Duration
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Alternative Duration names.
--
--------------------------------------------------------------------------------

module Payasan.Base.Names.Duration
  (

    drn_1
  , drn_2
  , drn_4
  , drn_8
  , drn_16
  , drn_32
  , drn_64
  , drn_128

  ) where

import Payasan.Base.Duration


-- Note - should duration expose Numeral

drn_1           :: Duration
drn_1           = d_whole

drn_2           :: Duration
drn_2           = d_half

drn_4           :: Duration
drn_4           = d_quarter

drn_8           :: Duration
drn_8           = d_eighth

drn_16          :: Duration
drn_16          = d_sixteenth

drn_32          :: Duration
drn_32          = d_thirty_secondth

drn_64          :: Duration
drn_64          = d_sixty_fourth

drn_128         :: Duration
drn_128         = d_one_hundred_and_twenty_eighth

