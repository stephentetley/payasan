{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo05 where

import Payasan.Base.Notelist

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Names.Interval

-- TEMP
import Payasan.Percussion.Internal.Base
import Payasan.Percussion.Internal.Parser
import Payasan.Base.Internal.LilyPond.Syntax hiding (middle_c)


-- DRUMS -- 

phrase01 :: GenLyPhrase DrumPitch
phrase01 = id $ 
    [drums| bda4 bd bd bda | bda |]



global_ri :: GlobalRenderInfo
global_ri = default_global_info { global_ly_octave_mode = RelPitch middle_c  }


manual_ri :: LocalRenderInfo
manual_ri = default_local_info { local_unit_note_len = UNIT_NOTE_4 }


{-

demo01 :: IO ()
demo01 = printAsLilyPond global_ri phrase01

demo02 :: IO ()
demo02 = printAsLilyPond global_ri $ mapPch (`addInterval` major_second) phrase01

demo03 :: IO ()
demo03 = printAsLilyPond global_ri $ augment phrase01

demo04 :: IO ()
demo04 = shellOutABC global_ri $ outputAsABC global_ri $ augment phrase01

demo05 :: IO ()
demo05 = shellOutLilyPond global_ri $ outputAsLilyPond global_ri $ augment phrase01

-}

-- test01 = barLength $ Meter 4 4