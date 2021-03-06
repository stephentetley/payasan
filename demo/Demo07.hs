{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo07 where

import Payasan.LilyPond.Lyricmode.Notelist

import qualified Payasan.Models.Lyrics.Internal.Base        as NEW  -- TEMP
import qualified Payasan.Models.Lyrics.Internal.Syllable    as NEW2  -- TEMP
import Payasan.Models.Lyrics.Internal.Plain


import Payasan.Base.Names.Duration

-- LYRICS -- 

-- NOTE - do not beam lyrics...


globals :: ShellInfo
globals = default_shell_info


locals :: SectionInfo
locals = default_section_info


-- Annos not handled (yet)...
--
-- Visually, quasiquote is definitely the most pleasing
-- representation...
--
phrase01 :: LyricPart1 ()
phrase01 = fromLilyPondWith default_score_info locals $ 
    [lyricmode| Shake4 ba8 -- by8 shake4 __ \skip4
              | Shake4 ba8 -- by8 shake.2
              |]

demo01  :: IO ()
demo01 = shellOutLilyPond globals $ outputAsLilyPond default_score_info $ phrase01



phrase01a :: LyricPart1 NEW.Stress
phrase01a = fromLilyPondWith default_score_info locals $ 
    [NEW.lyrics| Shake4\uns ba8\sec -- by8\uns shake2\primary
               | Shake4\uns ba8\sec -- by8\uns shake.2\pri               
               |]


demo01a  :: IO ()
demo01a = shellOutLilyPond globals $ NEW.outputAsLilyPond default_score_info $ phrase01a






-- demo02 :: IO ()
-- demo02 = printAsLilyPond default_score_info phrase01


phrase01b :: NEW.LyricsPart
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





-- Hey Ar-chae-op-ter-yx where d'you learn those tricks?


words01 :: NEW2.Lyrics
words01 =  
    [NEW2.lyrics| Hey Ar-chae-op-ter-yx where d'you learn those tricks?
                |]


-- Specifying lyrics as syllables with stress but no duration seems useful...

