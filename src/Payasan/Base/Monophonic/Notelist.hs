{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Notelist
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- (Pipeline)
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Notelist
  ( 

    module Payasan.Base.Internal.Shell
  , module Payasan.Base.Monophonic.Internal.Transform

  , Phrase
  , StdMonoPhrase
  , ABCMonoPhrase
  , abc
  , LyMonoPhrase
  , lilypond

  , ScoreInfo(..)        -- Re-export
  , default_score_info

  , VoiceInfo(..)
  , OctaveMode(..)
  , default_voice_info

  , LocalContextInfo(..)         -- Re-export
  , UnitNoteLength(..)
  , default_local_info


  , fromABC
  , fromABCWith
  , fromLilyPond
  , fromLilyPondWith

  , outputAsABC
  , printAsABC

  , MAIN.LilyPondPipeline(..)
  , MAIN.LilyPondPipeline2(..)
  , genOutputAsLilyPond
  , genOutputAsLilyPond2

  , outputAsLilyPond
  , printAsLilyPond

  , genOutputAsRhythmicMarkup
  , outputAsRhythmicMarkup
  , printAsRhythmicMarkup

  , ppRender

  , writeAsMIDI

  , outputAsTabular
  , printAsTabular

  , outputAsLinear
  , printAsLinear



  , mapPitch
  , mapDuration

  ) where

import Payasan.Base.Monophonic.Internal.ABCInTrans
import Payasan.Base.Monophonic.Internal.ABCParser (abc)
import Payasan.Base.Monophonic.Internal.LilyPondInTrans
import Payasan.Base.Monophonic.Internal.LilyPondQuasiquote (lilypond)
import Payasan.Base.Monophonic.Internal.LinearOutput
import Payasan.Base.Monophonic.Internal.MonoToMain
import Payasan.Base.Monophonic.Internal.Syntax
import Payasan.Base.Monophonic.Internal.TabularOutput
import Payasan.Base.Monophonic.Internal.Transform
import Payasan.Base.Monophonic.Internal.Traversals

import qualified Payasan.Base.Internal.LilyPond.RhythmicMarkup  as LY

import Payasan.Base.Internal.Output.Common ( LeafOutput(..) )
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.Shell
import qualified Payasan.Base.Notelist as MAIN

import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJClass        -- package: pretty


fromABC :: ABCMonoPhrase -> StdMonoPhrase
fromABC  = fromABCWith default_local_info

fromABCWith :: LocalContextInfo -> ABCMonoPhrase -> StdMonoPhrase
fromABCWith ri = abcTranslate . pushContextInfo ri


fromLilyPond :: VoiceInfo -> LyMonoPhrase () -> StdMonoPhrase
fromLilyPond infov = fromLilyPondWith infov default_local_info

fromLilyPondWith :: VoiceInfo 
                 -> LocalContextInfo 
                 -> LyMonoPhrase ()
                 -> StdMonoPhrase
fromLilyPondWith infov locals = 
    lilyPondTranslate infov . pushContextInfo locals


outputAsABC :: ScoreInfo -> VoiceInfo -> StdMonoPhrase -> String
outputAsABC infos infov = 
    MAIN.outputAsABC infos infov . translateToMain

printAsABC :: ScoreInfo -> VoiceInfo -> StdMonoPhrase -> IO ()
printAsABC infos infov = 
    MAIN.printAsABC infos infov . translateToMain



genOutputAsLilyPond :: MAIN.LilyPondPipeline p1 a1 p2 a2
                    -> Phrase p1 Duration a1
                    -> Doc
genOutputAsLilyPond config = MAIN.genOutputAsLilyPond config . translateToMain

genOutputAsLilyPond2 :: MAIN.LilyPondPipeline2 p1i a1i p2i a2i p1o a1o p2o a2o
                     -> Phrase p1i Duration a1i
                     -> Phrase p2i Duration a2i
                     -> Doc
genOutputAsLilyPond2 config ph1 ph2 = 
    MAIN.genOutputAsLilyPond2 config (translateToMain ph1) (translateToMain ph2)




outputAsLilyPond :: ScoreInfo -> VoiceInfo -> StdMonoPhrase -> String
outputAsLilyPond infos infov = 
    MAIN.outputAsLilyPond infos infov . translateToMain

printAsLilyPond :: ScoreInfo -> VoiceInfo -> StdMonoPhrase -> IO ()
printAsLilyPond infos infov = 
    MAIN.printAsLilyPond infos infov . translateToMain




genOutputAsRhythmicMarkup :: LY.MarkupOutput pch 
                          -> ScoreInfo 
                          -> VoiceInfo
                          -> Phrase pch Duration anno
                          -> Doc
genOutputAsRhythmicMarkup def infos infov = 
    MAIN.genOutputAsRhythmicMarkup def infos infov . translateToMain

outputAsRhythmicMarkup :: ScoreInfo -> VoiceInfo -> StdMonoPhrase -> String
outputAsRhythmicMarkup infos infov = 
    MAIN.outputAsRhythmicMarkup infos infov . translateToMain

printAsRhythmicMarkup :: ScoreInfo -> VoiceInfo -> StdMonoPhrase -> IO ()
printAsRhythmicMarkup infos infov = 
    MAIN.printAsRhythmicMarkup infos infov . translateToMain




ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdMonoPhrase -> IO ()
writeAsMIDI path = MAIN.writeAsMIDI path . translateToMain



outputAsTabular :: (Pretty pch, Pretty drn) 
                => ScoreInfo -> Phrase pch drn anno -> String
outputAsTabular _gi ph = ppRender $ monoTabular lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsTabular :: (Pretty pch, Pretty drn) 
               => ScoreInfo -> Phrase pch drn anno ->  IO ()
printAsTabular gi = putStrLn . outputAsTabular gi




outputAsLinear :: (Pretty pch, Pretty drn) 
                => ScoreInfo -> Phrase pch drn anno -> String
outputAsLinear _gi ph = ppRender $ monoLinear lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsLinear :: (Pretty pch, Pretty drn) 
               => ScoreInfo -> Phrase pch drn anno ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi
