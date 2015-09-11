{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.Output
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.Output
  ( 
    lilyPondOutput
  ) where

import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils
import Payasan.Base.Internal.RewriteMonad

import Text.PrettyPrint.HughesPJ        -- package: pretty



-- Do we need the monad? (code originally from ABC output where we did).

type Mon a = Rewrite Int a


lilyPondOutput :: GlobalRenderInfo -> LyPhrase -> Doc
lilyPondOutput globals ph = 
        header 
    $+$ block Nothing (modeBlockF $ (notes_header $+$ notes))
  where
    local1          = maybe default_local_info id $ firstRenderInfo ph
    header          = oHeader globals local1
    modeBlockF      = octaveModeBlock (global_ly_octave_mode globals)
    notes_header    = oPhraseHeader globals local1
    notes           = evalRewriteDefault (oLyPhrase ph) 0



oHeader :: GlobalRenderInfo -> LocalRenderInfo -> Doc
oHeader globals _locals = 
        version (global_ly_version globals)
    $+$ block (Just $ command "header") (title $ global_title globals)

oPhraseHeader :: GlobalRenderInfo -> LocalRenderInfo -> Doc
oPhraseHeader _globals locals = 
        key   (local_key locals)
    $+$ meter (local_meter locals)

octaveModeBlock :: OctaveMode -> Doc -> Doc
octaveModeBlock (AbsPitch)   d  = absolute $+$ d
octaveModeBlock (RelPitch p) d  = block (Just $ relative p) d


--------------------------------------------------------------------------------
-- Notelist


oLyPhrase :: LyPhrase -> Mon Doc
oLyPhrase (Phrase [])           = return empty
oLyPhrase (Phrase (x:xs))       = step (oBar x) xs
  where
    step d []     = return d
    step d (b:bs) = do { let ac = d <+> char '|' $+$ oBar b
                       ; step ac bs
                       }


oBar :: LyBar -> Doc
oBar (Bar _info cs) = hsep (map oCtxElement cs)



oCtxElement :: LyCtxElement -> Doc
oCtxElement (Atom e)            = oElement e
oCtxElement (Beamed cs)         = beamForm $ map oCtxElement cs
oCtxElement (Tuplet spec cs)    = tupletSpec spec <+> hsep (map oCtxElement cs)

oElement :: LyElement -> Doc
oElement (NoteElem n)           = note n
oElement (Rest d)               = rest d 
oElement (Chord ps d)           = chord ps d 
oElement (Graces xs)            = graces xs
