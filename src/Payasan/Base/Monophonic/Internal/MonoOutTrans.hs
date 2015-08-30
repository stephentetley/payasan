{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.MonoOutTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Translate Monophonic syntax to Main syntax.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.MonoOutTrans
  (
    translate
  ) where



import Payasan.Base.Monophonic.Internal.MonoSyntax

import Payasan.Base.Internal.ABCUtils
import qualified Payasan.Base.Internal.MainSyntax as T

import Payasan.Base.Duration
import Payasan.Base.Pitch



translate :: MonoPhrase pch drn -> T.Phrase pch drn
translate = phraseT


phraseT :: MonoPhrase pch drn -> T.Phrase pch drn
phraseT (MonoPhrase bs)          = T.Phrase $ map barT bs


barT :: Bar pch drn -> T.Bar pch drn
barT (Bar info cs)              = T.Bar info $ concatMap ctxElementT cs


-- | Remember - a beamed CtxElement may generate 1+ elements
--
ctxElementT :: CtxElement pch drn -> [T.CtxElement pch drn]
ctxElementT (Atom e)            = [T.Atom $ elementT e]
ctxElementT (Tuplet spec cs)    = [T.Tuplet spec $ concatMap ctxElementT cs]


elementT :: Element pch drn  -> T.Element pch drn
elementT (Note p d)            = T.NoteElem (T.Note p  d)
elementT (Rest d)              = T.Rest d

