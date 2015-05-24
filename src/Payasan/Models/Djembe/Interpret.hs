{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Djembe.Interpret
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Djembe - LMMS use sound font player and bank 128.
--
--------------------------------------------------------------------------------

module Payasan.Models.Djembe.Interpret
  (
    renderDjembePhrase
  , djembePhrase
  ) where


import Payasan.Models.Djembe.Base

import Payasan.Base
import Payasan.Base.Advance

import Control.Applicative



renderDjembePhrase :: StrokeF -> TrackData -> DjembePhrase a -> Track
renderDjembePhrase fn cfg phz = renderAdvance cfg $ getDjembePhrase phz fn

-- | A beat in Djembe system is an eighth not a quarter note
--
djembePhrase :: DjembePattern -> DjembePhrase ()
djembePhrase patt = DjembePhrase $ \tf ->
    eighth_note_length >>= \len ->
    mapM_ (render1 tf len) $ extrDjembePattern patt 
    

render1 :: StrokeF -> Seconds -> Wrapper -> Advance ()
render1 sf len (One e)          = renderNote sf len e
render1 sf len (Swing e)        = let len1 = 0.25 * len 
                                      len2 = 0.75 * len
                                  in advanceCursor len1 >> renderNote sf len2 e

render1 sf len (Flam a b)       = let len1 = 0.25 * len 
                                      len2 = 0.75 * len
                                  in renderNote sf len1 a >> renderNote sf len2 b

render1 sf len (Duplet a b)     = let len1 = 0.5 * len 
                                  in renderNote sf len1 a >> renderNote sf len1 b

render1 sf len (Triplet a b c)  = 
    let len1 = 2 * len * 0.333333 
    in renderNote sf len1 a >> renderNote sf len1 b >> renderNote sf len1 c


-- | Note chordFw_ is probably wrong place 
renderNote :: StrokeF -> Seconds -> Note -> Advance ()
renderNote sf len (N ac iden) = event_ len (sf ac iden)
renderNote sf len (C ac ps)   
    | null ps                 = return ()
    | otherwise               = chord_ len ps (\p -> sf ac p)

renderNote _  len (R)         = advanceCursor len

eighth_note_length :: ContextM m => m Seconds
eighth_note_length = (* 0.5) <$> quarter_note_length


