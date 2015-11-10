{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo07 where

import Payasan.LilyPond.Lyricmode.Notelist

import qualified Payasan.Models.Lyrics.Base   as NEW  -- TEMP
import Payasan.Models.Lyrics.Plain
import Payasan.Models.Lyrics.Monad

import Payasan.Base.Names.Duration

-- LYRICS -- 

-- NOTE - do not beam lyrics...


phrase01 :: LyricPhrase1 ()
phrase01 = fromLilyPondWith default_score_info locals $ 
    [lyricmode| Shake4 ba8 -- by8 shake2 | Shake4 ba8 -- by8 shake.2 |  |]




globals :: ShellInfo
globals = default_shell_info


locals :: LocalContextInfo
locals = default_local_info



demo01  :: IO ()
demo01 = shellOutLilyPond globals $ outputAsLilyPond default_score_info $ phrase01

-- demo02 :: IO ()
-- demo02 = printAsLilyPond default_score_info phrase01


phrase01b :: NEW.LyricsPhrase
phrase01b = fromLyrics $
    [ [ unstressed "Shake"      drn_4
      , secondary  "ba"         drn_8
      , hyphen
      , unstressed "by"         drn_8
      , primary    "shake"      drn_2
      ]
    , [ unstressed "Shake"      drn_4
      , secondary  "ba"         drn_8
      , hyphen
      , unstressed "by"         drn_8
      , primary    "shake."     drn_2
      ]
    ]

demo02  :: IO ()
demo02 = shellOutLilyPond globals $ NEW.outputAsLilyPond default_score_info $ phrase01b




phrase01c :: Lyrics ()
phrase01c = tell "Shake" drn_4 >> tell "ba-"   drn_8
         >> tell "by"    drn_8 >> tell "shake" drn_2


demo03  :: IO ()
demo03 = shellOutLilyPond globals $ NEW.outputAsLilyPond default_score_info $ fromLyricsM phrase01c

