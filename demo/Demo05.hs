{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo05 where

import Payasan.Percussion.Notelist

import Payasan.Base.Duration

-- TEMP
import Payasan.Percussion.Internal.Base
import Payasan.Percussion.Internal.Parser
import Payasan.Base.Internal.LilyPond.Syntax hiding (middle_c)



-- DRUMS -- 

phrase01 :: StdDrumPhrase
phrase01 = fromLilyPondWith global_ri manual_ri $ 
    [drums| bda4 bd bd bda | bda |]



global_ri :: GlobalRenderInfo
global_ri = default_global_info { global_ly_octave_mode = AbsPitch }


manual_ri :: LocalRenderInfo
manual_ri = default_local_info




demo01 :: IO ()
demo01 = printAsLilyPond global_ri phrase01

{-
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