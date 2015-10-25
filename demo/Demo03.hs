{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo03 where


import qualified Payasan.Models.Polyrhythms.Base as POLY

-- import Payasan.Base.Duration
import Payasan.Base.Notelist
import Payasan.Base.Pitch
import Payasan.Base.Names.Pitch

import Payasan.Base.Internal.LilyPond.Syntax (fromPitchRel, LyPitch)

import Payasan.Base.Names.Pitch

import Payasan.Base.Internal.Output.Common
import Payasan.Base.Internal.Output.Tabular.OutputBeam

import Data.Monoid ( (<>) )

-- POLYRHYTHM



globals :: ScoreInfo
globals = default_score_info 

voice :: VoiceInfo 
voice = default_voice_info { voice_ly_octave_mode = RelPitch middle_c }


locals :: LocalContextInfo
locals = default_local_info { local_unit_note_len = UNIT_NOTE_4 }


voiceA :: VoiceInfo 
voiceA = default_voice_info { voice_ly_octave_mode = RelPitch c_5 }

phraseA :: StdPhrase 
phraseA = fromLilyPondWith voiceA locals $ 
    [lilypond| e4 e \tuplet 3/2 { e e e } |]


voiceB :: VoiceInfo 
voiceB = default_voice_info { voice_ly_octave_mode = RelPitch middle_c }

phraseB :: StdPhrase 
phraseB = fromLilyPondWith voice locals $ 
    [lilypond| f4 f f f |]



demo01 :: IO ()
demo01 = shellOutLilyPond globals $ 
    POLY.outputAsLilyPond globals voiceA phraseA voiceB phraseB



debug01 :: IO StdPhrase
debug01 = fromLilyPondWithIO voice locals $ 
    [lilypond| c4 d e fis | c' |]



phrase01 :: StdPhrase 
phrase01 = fromLilyPondWith (voice { voice_ly_octave_mode = RelPitch c_5 }) locals $ 
    [lilypond| c4 d e fis | c' |]


demo01a :: IO ()
demo01a = printAsABC globals voice phrase01

demo01b :: IO ()
demo01b = printAsLilyPond globals voice phrase01


test01,test02, test03 :: Int
test01 = octaveCount $ Interval 9 13
test02 = octaveCount $ Interval 1 0
test03 = octaveCount $ Interval 8 12

test04 :: Interval
test04 = simpleIntervalOf $ Interval 8 12

test05 :: LyPitch
test05 = fromPitchRel c_5 fs_4

test06 = beamTabular std_ly_output $ [lilypond| c4 d e fis | c' |]


test07 :: LyPitch
test07 = fromPitchRel e_4 ff_4
