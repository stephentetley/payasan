{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Notelist
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- (Pipeline)
--
-- DEPRECATED (whole module)
--
--------------------------------------------------------------------------------

module Payasan.Score.Elementary.Notelist
  ( 

    module Payasan.PSC.Old.Shell
  , module Payasan.Score.Elementary.Internal.Transform

  , Section
  , StdElemSection
  , ABCElemSection
  , elementary_abc
  , LyElemSection1
  , elementary_lilypond

  , ScoreInfo(..)        -- Re-export
  , default_score_info


  , SectionInfo(..)         -- Re-export
  , UnitNoteLength(..)
  , default_section_info


--  , fromABC
--  , fromABCWith

--  , fromLilyPond_Relative
--  , fromLilyPondWith_Relative

--  , outputAsABC
--  , printAsABC

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

--  , writeAsMIDI

  , outputAsTabular
  , printAsTabular

  , outputAsLinear
  , printAsLinear



  , mapPitch
  , mapDuration

  ) where

import Payasan.Score.Elementary.Internal.ABCUnquote ( elementary_abc)
import Payasan.Score.Elementary.Internal.ElementaryToExternal
import Payasan.Score.Elementary.Internal.LilyPondUnquote ( elementary_lilypond )
import Payasan.Score.Elementary.Internal.ShowLinear
import Payasan.Score.Elementary.Internal.ShowTabular
import Payasan.Score.Elementary.Internal.Syntax
import Payasan.Score.Elementary.Internal.Transform
import Payasan.Score.Elementary.Internal.Traversals

import qualified Payasan.PSC.LilyPond.RhythmicMarkup  as LY

import Payasan.PSC.Base.ShowCommon ( LeafOutputNote(..) )
import Payasan.PSC.Old.Shell
import Payasan.PSC.Base.SyntaxCommon

import qualified Payasan.PSC.Old.Pipeline as MAIN

import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass        -- package: pretty

{-
fromABC :: ABCElemSection -> StdElemSection
fromABC  = fromABCWith default_section_info

fromABCWith :: SectionInfo -> ABCElemSection -> StdElemSection
fromABCWith locals = abcTranslate . pushSectionInfo locals


fromLilyPond_Relative :: Pitch -> LyElemSection1 () -> StdElemSection
fromLilyPond_Relative pch = fromLilyPondWith_Relative pch default_section_info

fromLilyPondWith_Relative :: Pitch 
                          -> SectionInfo 
                          -> LyElemSection1 ()
                          -> StdElemSection
fromLilyPondWith_Relative pch locals = 
    lilyPondTranslate_Relative pch . pushSectionInfo locals
-}

{-
outputAsABC :: ScoreInfo -> StaffInfo -> StdElemSection -> String
outputAsABC infos staff = 
    MAIN.outputAsABC infos staff . transElementaryToExternal

printAsABC :: ScoreInfo -> StaffInfo -> StdElemSection -> IO ()
printAsABC infos staff = 
    MAIN.printAsABC infos staff . transElementaryToExternal
-}


genOutputAsLilyPond :: MAIN.LilyPondPipeline p1 a1 p2 a2
                    -> Section p1 Duration a1
                    -> Doc
genOutputAsLilyPond config = 
    MAIN.genOutputAsLilyPond config . transElementaryToExternal

genOutputAsLilyPond2 :: MAIN.LilyPondPipeline2 p1i a1i p2i a2i p1o a1o p2o a2o
                     -> Section p1i Duration a1i
                     -> Section p2i Duration a2i
                     -> Doc
genOutputAsLilyPond2 config ph1 ph2 = 
    MAIN.genOutputAsLilyPond2 config (transElementaryToExternal ph1)
                                     (transElementaryToExternal ph2)




outputAsLilyPond_Relative :: ScoreInfo -> Pitch -> StdElemSection -> String
outputAsLilyPond_Relative infos pch = 
    MAIN.outputAsLilyPond_Relative infos pch . transElementaryToExternal


printAsLilyPond_Relative :: ScoreInfo ->Pitch -> StdElemSection -> IO ()
printAsLilyPond_Relative infos pch = 
    MAIN.printAsLilyPond_Relative infos pch . transElementaryToExternal




genOutputAsRhythmicMarkup :: LY.MarkupOutput pch 
                          -> ScoreInfo 
                          -> Section pch Duration anno
                          -> Doc
genOutputAsRhythmicMarkup def infos = 
    MAIN.genOutputAsRhythmicMarkup def infos . transElementaryToExternal

outputAsRhythmicMarkup :: ScoreInfo -> StdElemSection -> String
outputAsRhythmicMarkup infos = 
    MAIN.outputAsRhythmicMarkup infos . transElementaryToExternal

printAsRhythmicMarkup :: ScoreInfo -> StdElemSection -> IO ()
printAsRhythmicMarkup infos = 
    MAIN.printAsRhythmicMarkup infos . transElementaryToExternal




ppRender :: Doc -> String
ppRender = MAIN.ppRender


{-
writeAsMIDI :: FilePath -> StdElemSection -> IO ()
writeAsMIDI path = MAIN.writeAsMIDI path . transElementaryToExternal "noname"
-}


outputAsTabular :: (Pretty pch, Pretty drn) 
                => ScoreInfo -> Section pch drn anno -> String
outputAsTabular _gi ph = ppRender $ elemTabular lo ph
  where
    lo = LeafOutputNote { pp_pitch     = pPrint
                        , pp_duration  = pPrint
                        , pp_anno      = const empty
                        }

printAsTabular :: (Pretty pch, Pretty drn) 
               => ScoreInfo -> Section pch drn anno ->  IO ()
printAsTabular gi = putStrLn . outputAsTabular gi




outputAsLinear :: (Pretty pch, Pretty drn) 
                => ScoreInfo -> Section pch drn anno -> String
outputAsLinear _gi ph = ppRender $ elemLinear lo ph
  where
    lo = LeafOutputNote { pp_pitch     = pPrint
                        , pp_duration  = pPrint
                        , pp_anno      = const empty
                        }

printAsLinear :: (Pretty pch, Pretty drn) 
               => ScoreInfo -> Section pch drn anno ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi
