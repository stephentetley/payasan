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

import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty

-- Print two voices.
--
outputAsLilyPond :: ScoreInfo 
                 -> StaffInfo
                 -> Pitch -> MAIN.StdPhrase 
                 -> Pitch -> MAIN.StdPhrase 
                 -> String
outputAsLilyPond globals staff p1 rhya p2 rhyb = 
    MAIN.ppRender $ MAIN.genOutputAsLilyPond2 config2 rhya rhyb
  where
    config2         = MAIN.LilyPondPipeline2
                        { MAIN.pipe2_beam_trafo1   = addBeams
                        , MAIN.pipe2_out_trafo1    = LY.translateToOutput_Relative p1
                        , MAIN.pipe2_beam_trafo2   = addBeams
                        , MAIN.pipe2_out_trafo2    = LY.translateToOutput_Relative p2
                        , MAIN.pipe2_output_func   = polyrhythmScore globals staff p1 p2
                        }



--------------------------------------------------------------------------------
-- Output


-- TODO - ideally key and time signature should form a common prefix...
--
polyrhythmScore :: Anno a1 
                => ScoreInfo 
                -> StaffInfo
                -> Pitch
                -> Pitch
                -> BEAM.Phrase LyPitch LyNoteLength a1 
                -> BEAM.Phrase LyPitch LyNoteLength a1
                -> Doc
polyrhythmScore globals staff p1 p2 ph1 ph2 = 
        header $+$ newStaff_ <+> (simultaneous1 (startphrase $+$ upper $+$ lower))
  where
    header      = scoreHeader globals
    locals1     = maybe default_local_info id $ BEAM.firstContextInfo ph1
    startphrase = oPhraseHeader staff locals1
    upper       = newVoiceDefn "first" $+$ anonBlock (command "voiceOne" <+> upper1)
    upper1      = polyVoice_Relative def p1 locals1 ph1
    lower       = newVoiceDefn "second" $+$ anonBlock (command "voiceTwo" <+> lower1)
    lower1      = polyVoice_Relative def p2 locals1 ph2
    def         = LyOutputDef { printPitch = pitch, printAnno = anno }



oPhraseHeader :: StaffInfo -> LocalContextInfo -> Doc
oPhraseHeader staff locals = 
        clef_ (staff_clef staff)
    $+$ key_  (local_key locals)
    $+$ time_ (local_meter locals)


polyVoice_Relative :: LyOutputDef pch anno 
                   -> Pitch
                   -> LocalContextInfo
                   -> GenLyPhrase pch anno -> Doc
polyVoice_Relative def pch locals ph = 
    block (Just $ relative_ pch) notes
  where
    notes           = lilypondNotes def locals ph

