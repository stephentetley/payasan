{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.ElementaryToExternal
-- Copyright   :  (c) Stephen Tetley 2015-2016
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
    transElementaryToExternal
  , chord_transElementaryToExternal
  ) where


import Payasan.Score.Elementary.Internal.Syntax

import qualified Payasan.PSC.Repr.External.Syntax as T

import Payasan.PSC.Base.SyntaxCommon



transElementaryToExternal :: forall pch drn anno.
                             String -> Part pch drn anno -> T.Part pch drn anno
transElementaryToExternal name      = partT
  where
    partT :: Part pch drn anno -> T.Part pch drn anno
    partT (Part info bs)            = T.Part [T.Section name info $ map barT bs]


    barT :: Bar pch drn anno -> T.Bar pch drn anno
    barT (Bar cs)                   = T.Bar $ map noteGroupT cs

    -- | No beams in Elementary syntax so one-to-one
    --
    noteGroupT :: NoteGroup pch drn anno -> T.NoteGroup pch drn anno
    noteGroupT (Atom e)             = T.Atom $ elementT e
    noteGroupT (Tuplet spec es)     = T.Tuplet spec $ map (T.Atom . elementT) es


    elementT :: Element pch drn anno -> T.Element pch drn anno
    elementT (Note p d a t)         = T.NoteElem (T.Note p d) a t
    elementT (Rest d)               = T.Rest d
    elementT (Spacer d)             = T.Spacer d
    elementT (Skip d)               = T.Skip d
    elementT (Punctuation s)        = T.Punctuation s



-- | Note - Prevents type change on duration (ideally duration 
-- would be opaque, it cannot be with the Main and Elementary
-- representations).
--
chord_transElementaryToExternal :: forall pch drn anno. 
                                   String
                                -> Part [pch] drn anno
                                -> T.Part pch drn anno
chord_transElementaryToExternal name = partT
  where
    partT :: Part [pch] drn anno -> T.Part pch drn anno
    partT (Part info bs)            = T.Part [T.Section name info $ map barT bs]


    barT :: Bar [pch] drn anno -> T.Bar pch drn anno
    barT (Bar cs)                   = T.Bar $ map noteGroupT cs


    noteGroupT :: NoteGroup [pch] drn anno -> T.NoteGroup pch drn anno
    noteGroupT (Atom e)             = T.Atom $ elementT e
    noteGroupT (Tuplet spec es)     = T.Tuplet spec $ map (T.Atom . elementT) es


    elementT :: Element [pch] drn anno -> T.Element pch drn anno
    elementT (Note p d a t)         = 
        case p of 
          []  -> T.Rest d
          [x] -> T.NoteElem (T.Note x d) a t
          xs  -> T.Chord xs d a t

    elementT (Rest d)               = T.Rest d
    elementT (Spacer d)             = T.Spacer d
    elementT (Skip d)               = T.Skip d
    elementT (Punctuation s)        = T.Punctuation s
