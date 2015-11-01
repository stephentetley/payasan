{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo07 where

import Payasan.LilyPond.Lyricmode.Notelist
import qualified Payasan.Models.Lyrics.Base  as NEW  -- TEMP
import Payasan.Models.Lyrics.Plain



-- LYRICS -- 

-- NOTE - do not beam lyrics...


phrase01 :: StdLyricPhrase
phrase01 = fromLilyPondWith default_score_info locals $ 
    [lyricmode| Shake4 ba8 -- by8 shake2 | Shake4 ba8 -- by8 shake.2 |  |]




globals :: ShellInfo
globals = default_shell_info


locals :: LocalContextInfo
locals = default_local_info



demo01  :: IO ()
demo01 = shellOutLilyPond globals $ outputAsLilyPond default_score_info $ phrase01

demo02 :: IO ()
demo02 = printAsLilyPond default_score_info phrase01



