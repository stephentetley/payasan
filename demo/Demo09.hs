{-# LANGUAGE QuasiQuotes                #-}
{-# OPTIONS -Wall #-}

module Demo09 where

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Names.Pitch


import Payasan.LilyPond.Cadenza.Notelist
import qualified Payasan.LilyPond.Cadenza.Internal.CadenzaToMono  as TEMP
import qualified Payasan.LilyPond.Cadenza.Internal.Transform      as TEMP


-- MONOPHONIC

locals :: SectionInfo
locals = default_section_info

globals :: ScoreInfo
globals = default_score_info 




-- Note - MonoPhrase reads (and ignores) beam group brackets.
-- Beams are re-synthesized in the output.
--
phrase01 :: StdCadenzaPhrase
phrase01 = fromLilyPondWith_Relative c_5 locals $ 
    [cadenza| c4 c d8[d d] f4 g4. |]



demo01 :: IO ()
demo01 = shellOutLilyPond default_shell_info $ 
    outputAsLilyPond_Relative globals c_5 $ phrase01


