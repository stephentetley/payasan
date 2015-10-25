{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Polyrhythms.Base
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Polyrhythms
--
--------------------------------------------------------------------------------

module Payasan.Models.Polyrhythms.Base
  ( 

    outputAsLilyPond

  ) where


import qualified Payasan.Base.Internal.LilyPond.OutTrans        as LY
import Payasan.Base.Internal.LilyPond.SimpleOutput
import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils

import Payasan.Base.Internal.AddBeams
import qualified Payasan.Base.Internal.BeamSyntax               as BEAM
import Payasan.Base.Internal.CommonSyntax
import qualified Payasan.Base.Internal.Pipeline                 as MAIN

import Text.PrettyPrint.HughesPJClass           -- package: pretty

-- Print two voices.
--
outputAsLilyPond :: ScoreInfo 
                 -> VoiceInfo -> MAIN.StdPhrase 
                 -> VoiceInfo -> MAIN.StdPhrase 
                 -> String
outputAsLilyPond globals infoa rhya infob rhyb = 
    MAIN.ppRender $ MAIN.genOutputAsLilyPond2 config2 rhya rhyb
  where
    config2         = MAIN.LilyPondPipeline2
                        { MAIN.pipe2_beam_trafo1   = addBeams
                        , MAIN.pipe2_out_trafo1    = LY.translateToOutput infoa
                        , MAIN.pipe2_beam_trafo2   = addBeams
                        , MAIN.pipe2_out_trafo2    = LY.translateToOutput infob
                        , MAIN.pipe2_output_func   = polyrhythmScore globals infoa infob
                        }



--------------------------------------------------------------------------------
-- Output


-- TODO - ideally key and time signature should be a common prefix...
--
polyrhythmScore :: Anno a1 
                => ScoreInfo 
                -> VoiceInfo
                -> VoiceInfo
                -> BEAM.Phrase LyPitch LyNoteLength a1 
                -> BEAM.Phrase LyPitch LyNoteLength a1
                -> Doc
polyrhythmScore globals infoa infob ph1 ph2 = 
        header $+$ newStaff <+> simultaneous1 (upper $+$ lower)
  where
    header      = scoreHeader globals
    upper       = newVoiceDefn "first" $+$ anonBlock (command "voiceOne" <+> upper1)
    upper1      = simpleVoice def infoa ph1
    lower       = newVoiceDefn "second" $+$ anonBlock (command "voiceTwo" <+> lower1)
    lower1      = simpleVoice def infob ph2
    def         = LyOutputDef { printPitch = pitch, printAnno = anno }