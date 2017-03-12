{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.ElementaryToExternal
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Elementary syntax to External (Main) syntax (pipline output 
-- from External syntax).
--
--------------------------------------------------------------------------------

module Payasan.Score.Elementary.Internal.ElementaryToExternal
  (
    fromElementary

  , transElementaryToExternal           -- old API
  , chord_transElementaryToExternal
  ) where


import Payasan.Score.Elementary.Internal.Syntax

import qualified Payasan.PSC.Repr.External.Syntax as T



fromElementary :: Section pch drn anno -> T.Section pch drn anno
fromElementary = sectionT



transElementaryToExternal :: forall pch drn anno.
                             Section pch drn anno -> T.Part pch drn anno
transElementaryToExternal s1 = T.Part { T.part_sections = [sectionT s1] }
 
sectionT :: Section pch drn anno -> T.Section pch drn anno
sectionT (Section name info bs)      = T.Section name info $ map barT bs
  where

    barT :: Bar pch drn anno -> T.Bar pch drn anno
    barT (Bar cs)                   = T.Bar $ map noteGroupT cs

    -- | No beams in Elementary syntax so one-to-one
    --
    noteGroupT :: NoteGroup pch drn anno -> T.NoteGroup pch drn anno
    noteGroupT (Atom e)             = T.Atom $ elementT e
    noteGroupT (Tuplet spec es)     = T.Tuplet spec $ map (T.Atom . elementT) es


    elementT :: Element pch drn anno -> T.Element pch drn anno
    elementT (Note p d a t)         = T.Note p d a t
    elementT (Rest d)               = T.Rest d
    elementT (Spacer d)             = T.Spacer d
    elementT (Skip d)               = T.Skip d
    elementT (Punctuation s)        = T.Punctuation s



chord_transElementaryToExternal :: forall pch drn anno. 
                                   Section [pch] drn anno
                                -> T.Part pch drn anno
chord_transElementaryToExternal = sectionT1
  where
    sectionT1 :: Section [pch] drn anno -> T.Part pch drn anno
    sectionT1 (Section name info bs) = T.Part [T.Section name info $ map barT bs]


    barT :: Bar [pch] drn anno -> T.Bar pch drn anno
    barT (Bar cs)                   = T.Bar $ map noteGroupT cs


    noteGroupT :: NoteGroup [pch] drn anno -> T.NoteGroup pch drn anno
    noteGroupT (Atom e)             = T.Atom $ elementT e
    noteGroupT (Tuplet spec es)     = T.Tuplet spec $ map (T.Atom . elementT) es


    elementT :: Element [pch] drn anno -> T.Element pch drn anno
    elementT (Note p d a t)         = 
        case p of 
          []  -> T.Rest d
          [x] -> T.Note x d a t
          xs  -> T.Chord xs d a t

    elementT (Rest d)               = T.Rest d
    elementT (Spacer d)             = T.Spacer d
    elementT (Skip d)               = T.Skip d
    elementT (Punctuation s)        = T.Punctuation s
