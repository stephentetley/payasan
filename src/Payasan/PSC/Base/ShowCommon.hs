{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Base.ShowCommon
-- Copyright   :  (c) Stephen Tetley 2015-2017
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

module Payasan.PSC.Base.ShowCommon
  ( 

    LeafOutputNote(..)
  , LeafOutputEvent(..)

  , pitch_duration_output

  ) where



import Payasan.Base.Duration
import Payasan.Base.Pitch



import Text.PrettyPrint.HughesPJClass           -- package: pretty

data LeafOutputNote pch drn anno = LeafOutputNote
    { pp_pitch          :: pch -> Doc
    , pp_duration       :: drn -> Doc 
    , pp_anno           :: anno -> Doc
    }

data LeafOutputEvent onset pch drn anno = LeafOutputEvent
    { pp_onset          :: onset -> Doc
    , pp_event          :: pch -> drn -> anno -> Doc
    , pp_event_grace    :: pch -> drn -> Doc
    }




pitch_duration_output :: LeafOutputNote Pitch Duration anno
pitch_duration_output = 
    LeafOutputNote { pp_pitch     = pPrint
                   , pp_duration  = pPrint
                   , pp_anno      = const empty
                   }

