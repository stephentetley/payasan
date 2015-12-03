{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.ElementaryToMain
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Elementary syntax to Main syntax (pipline output 
-- from Main syntax).
--
--------------------------------------------------------------------------------

module Payasan.Base.Elementary.Internal.ElementaryToMain
  (
    translateToMain
  , chordTranslateToMain
  ) where


import Payasan.Base.Elementary.Internal.Syntax

import Payasan.Base.Internal.CommonSyntax
import qualified Payasan.Base.Internal.MainSyntax as T



translateToMain :: forall pch drn anno.
                   Phrase pch drn anno -> T.Phrase pch drn anno
translateToMain = phraseT
  where
    phraseT :: Phrase pch drn anno -> T.Phrase pch drn anno
    phraseT (Phrase info bs)        = T.Phrase $ map (barT info) bs


    barT :: SectionInfo -> Bar pch drn anno -> T.Bar pch drn anno
    barT info (Bar cs)              = T.Bar info $ map noteGroupT cs

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
chordTranslateToMain :: forall pch drn anno. 
                      Phrase [pch] drn anno
                   -> T.Phrase pch drn anno
chordTranslateToMain = phraseT
  where
    phraseT :: Phrase [pch] drn anno -> T.Phrase pch drn anno
    phraseT (Phrase info bs)        = T.Phrase $ map (barT info) bs


    barT :: SectionInfo -> Bar [pch] drn anno -> T.Bar pch drn anno
    barT info (Bar cs)              = T.Bar info $ map noteGroupT cs


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
