{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Backend.Output.Common
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output to console either in a linear or Humdrum-like form.
--
-- This is intended debugging and checking purposes.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Backend.Output.Common
  ( 

    LeafOutput(..)

  , std_ly_output
  , lyNoteLength
  , lyPitch

  , std_abc_output
  , abcNoteLength
  , abcPitch

  , pitch_duration_output

  ) where


import qualified Payasan.PSC.Backend.LilyPond.Syntax    as LY
import qualified Payasan.PSC.Backend.ABC.Syntax         as ABC

import Payasan.Base.Duration
import Payasan.Base.Pitch



import Text.PrettyPrint.HughesPJClass           -- package: pretty

data LeafOutput pch drn anno = LeafOutput 
    { pp_pitch          :: pch -> Doc
    , pp_duration       :: drn -> Doc 
    , pp_anno           :: anno -> Doc
    }



std_ly_output :: LeafOutput LY.LyPitch LY.LyNoteLength ()
std_ly_output = LeafOutput { pp_pitch     = lyPitch
                           , pp_duration  = lyNoteLength
                           , pp_anno      = const empty
                           }

lyNoteLength :: LY.LyNoteLength -> Doc
lyNoteLength (LY.DrnDefault)    = char '.'
lyNoteLength (LY.DrnExplicit d) = duration d

lyPitch :: LY.LyPitch -> Doc
lyPitch = pPrint


std_abc_output :: LeafOutput ABC.ABCPitch ABC.ABCNoteLength ()
std_abc_output = LeafOutput { pp_pitch     = abcPitch
                            , pp_duration  = abcNoteLength
                            , pp_anno      = const empty
                            }

abcNoteLength :: ABC.ABCNoteLength -> Doc
abcNoteLength (ABC.DNL)         = char '.'
abcNoteLength d                 = pPrint d

abcPitch :: ABC.ABCPitch -> Doc
abcPitch = pPrint


pitch_duration_output :: LeafOutput Pitch Duration anno
pitch_duration_output = LeafOutput { pp_pitch     = pPrint
                                   , pp_duration  = pPrint
                                   , pp_anno      = const empty
                                   }


duration :: Duration -> Doc
duration d = 
    maybe (text "!zero") (\(n,dc) -> fn n <> dots dc) $ symbolicComponents d 
  where
    dots dc     = text $ replicate dc '.'
    fn Maxima   = text "maxima"
    fn Longa    = text "longa"
    fn Breve    = text "breve"
    fn D1       = int 1
    fn D2       = int 2
    fn D4       = int 4
    fn D8       = int 8
    fn D16      = int 16
    fn D32      = int 32
    fn D64      = int 64
    fn D128     = int 128



