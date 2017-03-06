{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Cadenza.Internal.LilyPondUnquote
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser for LilyPond'\s unmetered Cadenza mode. 
--
--------------------------------------------------------------------------------

module Payasan.Score.Cadenza.Internal.LilyPondUnquote
  (

    cadenza

  ) where


import Payasan.Score.Cadenza.Internal.LilyPondParser
import Payasan.Score.Cadenza.Internal.Syntax

import Payasan.PSC.Base.SyntaxCommon


import Text.Parsec                              -- package: parsec

import Language.Haskell.TH.Quote                -- package: template-haskell




--------------------------------------------------------------------------------
-- Quasiquote

cadenza :: QuasiQuoter
cadenza = QuasiQuoter
    { quoteExp = \s -> case parseCadenzaNoAnno s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 

