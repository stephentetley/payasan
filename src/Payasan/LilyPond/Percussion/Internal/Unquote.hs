{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Percussion.Internal.Unquote
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Unquote.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.Percussion.Internal.Unquote
  (
    drummode
  ) where

import Payasan.LilyPond.Percussion.Internal.Parser



import Language.Haskell.TH.Quote                -- package: template-haskell



--------------------------------------------------------------------------------
-- Quasiquote

-- Design note - use the lilypond mode command name @\drums\ as the 
-- quasiquoter name.

drummode :: QuasiQuoter
drummode = QuasiQuoter
    { quoteExp = \s -> case parseLyDrums s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 

