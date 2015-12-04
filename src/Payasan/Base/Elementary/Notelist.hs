{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Notelist
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

module Payasan.Base.Elementary.Notelist
  ( 

    module Payasan.Base.Internal.Shell
  , module Payasan.Base.Elementary.Internal.Transform

  , Phrase
  , StdElemPhrase
  , ABCElemPhrase
  , abc
  , LyElemPhrase1
  , lilypond

  , ScoreInfo(..)        -- Re-export
  , default_score_info

  , StaffInfo(..)
  , default_staff_info

  , SectionInfo(..)         -- Re-export
  , UnitNoteLength(..)
  , default_section_info


  , fromABC
  , fromABCWith

  , fromLilyPond_Relative
  , fromLilyPondWith_Relative

  , outputAsABC
  , printAsABC

  , MAIN.LilyPondPipeline(..)
  , MAIN.LilyPondPipeline2(..)
  , genOutputAsLilyPond
  , genOutputAsLilyPond2

  , outputAsLilyPond_Relative
  , printAsLilyPond_Relative

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

import Payasan.Base.Elementary.Internal.ABCInTrans
import Payasan.Base.Elementary.Internal.ABCParser (abc)
import Payasan.Base.Elementary.Internal.ElementaryToMain
import Payasan.Base.Elementary.Internal.LilyPondInTrans
import Payasan.Base.Elementary.Internal.LilyPondParser (lilypond)
import Payasan.Base.Elementary.Internal.LinearOutput
import Payasan.Base.Elementary.Internal.Syntax
import Payasan.Base.Elementary.Internal.TabularOutput
import Payasan.Base.Elementary.Internal.Transform
import Payasan.Base.Elementary.Internal.Traversals

import qualified Payasan.Base.Internal.LilyPond.RhythmicMarkup  as LY

import Payasan.Base.Internal.Output.Common ( LeafOutput(..) )
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.Shell
import qualified Payasan.Base.Notelist as MAIN

import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass        -- package: pretty


fromABC :: ABCElemPhrase -> StdElemPhrase
fromABC  = fromABCWith default_section_info

fromABCWith :: SectionInfo -> ABCElemPhrase -> StdElemPhrase
fromABCWith locals = abcTranslate . pushSectionInfo locals


fromLilyPond_Relative :: Pitch -> LyElemPhrase1 () -> StdElemPhrase
fromLilyPond_Relative pch = fromLilyPondWith_Relative pch default_section_info

fromLilyPondWith_Relative :: Pitch 
                          -> SectionInfo 
                          -> LyElemPhrase1 ()
                          -> StdElemPhrase
fromLilyPondWith_Relative pch locals = 
    lilyPondTranslate_Relative pch . pushSectionInfo locals


outputAsABC :: ScoreInfo -> StaffInfo -> StdElemPhrase -> String
outputAsABC infos staff = 
    MAIN.outputAsABC infos staff . translateToMain

printAsABC :: ScoreInfo -> StaffInfo -> StdElemPhrase -> IO ()
printAsABC infos staff = 
    MAIN.printAsABC infos staff . translateToMain



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




outputAsLilyPond_Relative :: ScoreInfo -> Pitch -> StdElemPhrase -> String
outputAsLilyPond_Relative infos pch = 
    MAIN.outputAsLilyPond_Relative infos pch . translateToMain


printAsLilyPond_Relative :: ScoreInfo -> Pitch -> StdElemPhrase -> IO ()
printAsLilyPond_Relative infos pch = 
    MAIN.printAsLilyPond_Relative infos pch . translateToMain




genOutputAsRhythmicMarkup :: LY.MarkupOutput pch 
                          -> ScoreInfo 
                          -> Phrase pch Duration anno
                          -> Doc
genOutputAsRhythmicMarkup def infos = 
    MAIN.genOutputAsRhythmicMarkup def infos . translateToMain

outputAsRhythmicMarkup :: ScoreInfo -> StdElemPhrase -> String
outputAsRhythmicMarkup infos = 
    MAIN.outputAsRhythmicMarkup infos . translateToMain

printAsRhythmicMarkup :: ScoreInfo -> StdElemPhrase -> IO ()
printAsRhythmicMarkup infos = 
    MAIN.printAsRhythmicMarkup infos . translateToMain




ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdElemPhrase -> IO ()
writeAsMIDI path = MAIN.writeAsMIDI path . translateToMain



outputAsTabular :: (Pretty pch, Pretty drn) 
                => ScoreInfo -> Phrase pch drn anno -> String
outputAsTabular _gi ph = ppRender $ elemTabular lo ph
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
outputAsLinear _gi ph = ppRender $ elemLinear lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsLinear :: (Pretty pch, Pretty drn) 
               => ScoreInfo -> Phrase pch drn anno ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi