{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.Polyrhythms.Base
-- Copyright   :  (c) Stephen Tetley 2015-2017
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

--    outputAsLilyPond
--  , outputTimbalesStyle

  ) where

import Payasan.LilyPond.Percussion.Internal.Base

import Payasan.PSC.LilyPond.Base
import qualified Payasan.PSC.LilyPond.OutTrans        as LY
import Payasan.PSC.LilyPond.SimpleOutput
import Payasan.PSC.LilyPond.Pretty

import Payasan.PSC.Repr.External.AddBeams
import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Basis
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty

{-

-- Print two voices.
--
outputAsLilyPond :: String      
                 -> String
                 -> String
                 -> Pitch -> MAIN.Part Pitch Duration ()
                 -> Pitch -> MAIN.Part Pitch Duration ()
                 -> String
outputAsLilyPond lyversion title clefname p1 ph1 p2 ph2 = 
    MAIN.ppRender $ MAIN.genOutputAsLilyPond2 config2 ph1 ph2
  where
    config2         = MAIN.LilyPondPipeline2
                        { MAIN.pipe2_beam_trafo1   = addBeams
                        , MAIN.pipe2_out_trafo1    = LY.translateToLyPartOut_Relative p1
                        , MAIN.pipe2_beam_trafo2   = addBeams
                        , MAIN.pipe2_out_trafo2    = LY.translateToLyPartOut_Relative p2
                        , MAIN.pipe2_output_func   = polyrhythmScore lyversion title clefname p1 p2
                        }



outputTimbalesStyle :: String 
                    -> String
                    -> MAIN.Part Pitch Duration ()
                    -> MAIN.Part Pitch Duration ()
                    -> String
outputTimbalesStyle lyversion title ph1 ph2 =
    MAIN.ppRender $ MAIN.genOutputAsLilyPond2 config2 ph1 ph2
  where
    config2         = MAIN.LilyPondPipeline2
                        { MAIN.pipe2_beam_trafo1   = addBeams
                        , MAIN.pipe2_out_trafo1    = timbalesTrafo Hitimbale
                        , MAIN.pipe2_beam_trafo2   = addBeams
                        , MAIN.pipe2_out_trafo2    = timbalesTrafo Lotimbale
                        , MAIN.pipe2_output_func   = timbalesStyle lyversion title
                        }
-}


timbalesTrafo :: DrumPitch 
              -> Part pch Duration a 
              -> Part DrumPitch LyNoteLength a
timbalesTrafo pch = 
    transformExternal (drumnote_algo pch) . LY.translateToLyPartOut_DurationOnly



drumnote_algo :: DrumPitch -> ExternalAlgo () pch DrumPitch drn drn anno anno
drumnote_algo pch = ExternalAlgo
    { initial_state     = ()
    , element_trafo     = liftElementTrafo $ elementP pch
    }


elementP :: forall pch drn anno. 
            DrumPitch
         -> Element pch drn anno 
         -> Element DrumPitch drn anno
elementP dpitch elt = case elt of 
    Note _ d a t        -> Note dpitch d a t
    Rest d              -> Rest d
    Spacer d            -> Spacer d
    Skip d              -> Skip d
    Chord _ d a t       -> Note dpitch d a t

    Graces ns           -> Graces $ map grace1P ns
    Punctuation s       -> Punctuation s
  where
    grace1P :: Grace1 pch drn -> Grace1 DrumPitch drn
    grace1P (Grace1 _ d)        = Grace1 dpitch d


--------------------------------------------------------------------------------
-- Output


-- Key and time signature form a common prefix...
--
polyrhythmScore :: Anno a1 
                => String
                -> String
                -> String
                -> Pitch
                -> Pitch
                -> Part LyPitch LyNoteLength a1 
                -> Part LyPitch LyNoteLength a1
                -> Doc
polyrhythmScore lyversion title clefname p1 p2 ph1 ph2 = 
        header $+$ newStaff_ <+> (simultaneous1 (startphrase $+$ upper $+$ lower))
  where
    header      = scoreHeader lyversion title
    locals1     = initialSectionInfo ph1
    startphrase = oPartHeader clefname locals1
    upper       = newVoiceDefn "upper" $+$ anonBlock (command "voiceOne" <+> upper1)
    upper1      = polyVoice_Relative def p1 locals1 ph1
    lower       = newVoiceDefn "lower" $+$ anonBlock (command "voiceTwo" <+> lower1)
    lower1      = polyVoice_Relative def p2 locals1 ph2
    def         = LyOutputDef { printPitch = pitch, printAnno = anno }


-- TODO - avoid extra line for unmetered...
--
oPartHeader :: String -> SectionInfo -> Doc
oPartHeader clefname locals = 
        clef_ clefname
    $+$ key_  (section_key locals)
    $+$ case section_meter locals of Unmetered -> empty
                                     Metered t -> time_ t


polyVoice_Relative :: LyOutputDef pch anno 
                   -> Pitch
                   -> SectionInfo
                   -> Part pch LyNoteLength anno -> Doc
polyVoice_Relative def pch locals ph = 
    block (Just $ relative_ pch) (extractDoc notes)
  where
    notes           = lilypondNoteList def locals ph



--------------------------------------------------------------------------------
-- Timbales style output


timbalesStyle :: Anno a1 
              => String
              -> String
              -> Part DrumPitch LyNoteLength a1 
              -> Part DrumPitch LyNoteLength a1
              -> Doc
timbalesStyle lyversion title ph1 ph2 = 
        header $+$ upper_def $+$ lower_def $+$ score_ score
  where
    header      = scoreHeader lyversion title
    locals1     = initialSectionInfo ph1
    upper_def   = phraseDef "upper" locals1 ph1
    lower_def   = phraseDef "lower" locals1 ph2

    score       =     newDrumStaffWith_ overrides 
                  $+$ simultaneous1 (upper_voice $+$ lower_voice)

    overrides   = vcat $ [ definition "drumStyleTable" $ value "timbales-style"
                         , override_ "StaffSymbol.line-count = #2"
                         , override_ "StaffSymbol.staff-space = #(magstep 3)"
                         ]

    upper_voice = newDrumVoice_ <+> anonBlock (stemUp_ <+> command "upper")
    lower_voice = newDrumVoice_ <+> anonBlock (stemDown_ <+> command "lower")


phraseDef :: Anno anno
          => String -> SectionInfo -> Part DrumPitch LyNoteLength anno -> Doc
phraseDef name locals ph = 
    definition name $ polyVoice_Drum locals ph


polyVoice_Drum :: Anno anno
               => SectionInfo -> Part DrumPitch LyNoteLength anno -> Doc
polyVoice_Drum locals ph = 
    block (Just $ drummode_) (extractDoc notes)
  where
    notes           = lilypondNoteList def locals ph
    def             = LyOutputDef { printPitch = pPrint, printAnno = anno }
