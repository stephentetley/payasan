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

  , GlobalRenderInfo(..)        -- Re-export
  , OctaveMode(..)
  , default_global_info

  , LocalRenderInfo(..)         -- Re-export
  , UnitNoteLength(..)
  , default_local_info


  , fromABC
  , fromABCWith
  , fromLilyPond
  , fromLilyPondWith

  , outputAsABC
  , printAsABC

  , genOutputAsLilyPond
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

import Payasan.Base.Internal.LilyPond.Output (LyOutputDef(..))
import qualified Payasan.Base.Internal.LilyPond.RhythmicMarkup  as RHY

import Payasan.Base.Internal.Output.Common ( LeafOutput(..) )
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.Shell
import qualified Payasan.Base.Notelist as MAIN

import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJClass        -- package: pretty


fromABC :: ABCMonoPhrase -> StdMonoPhrase
fromABC  = fromABCWith default_local_info

fromABCWith :: LocalRenderInfo -> ABCMonoPhrase -> StdMonoPhrase
fromABCWith ri = abcTranslate . pushLocalRenderInfo ri


fromLilyPond :: GlobalRenderInfo -> LyMonoPhrase () -> StdMonoPhrase
fromLilyPond gi = fromLilyPondWith gi default_local_info

fromLilyPondWith :: GlobalRenderInfo 
                 -> LocalRenderInfo 
                 -> LyMonoPhrase ()
                 -> StdMonoPhrase
fromLilyPondWith gi ri = lilyPondTranslate gi . pushLocalRenderInfo ri


outputAsABC :: GlobalRenderInfo -> StdMonoPhrase -> String
outputAsABC gi = MAIN.outputAsABC gi . translateToMain

printAsABC :: GlobalRenderInfo -> StdMonoPhrase -> IO ()
printAsABC gi = MAIN.printAsABC gi . translateToMain


genOutputAsLilyPond :: LyOutputDef pch anno 
                    -> GlobalRenderInfo 
                    -> Phrase pch Duration anno
                    -> String
genOutputAsLilyPond def gi = MAIN.genOutputAsLilyPond def gi . translateToMain

outputAsLilyPond :: GlobalRenderInfo -> StdMonoPhrase -> String
outputAsLilyPond gi = MAIN.outputAsLilyPond gi . translateToMain

printAsLilyPond :: GlobalRenderInfo -> StdMonoPhrase -> IO ()
printAsLilyPond gi = MAIN.printAsLilyPond gi . translateToMain


genOutputAsRhythmicMarkup :: RHY.MarkupOutput pch 
                          -> GlobalRenderInfo 
                          -> Phrase pch Duration anno
                          -> String
genOutputAsRhythmicMarkup def gi = MAIN.genOutputAsRhythmicMarkup def gi . translateToMain

outputAsRhythmicMarkup :: GlobalRenderInfo -> StdMonoPhrase -> String
outputAsRhythmicMarkup gi = MAIN.outputAsRhythmicMarkup gi . translateToMain

printAsRhythmicMarkup :: GlobalRenderInfo -> StdMonoPhrase -> IO ()
printAsRhythmicMarkup gi = MAIN.printAsRhythmicMarkup gi . translateToMain


ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdMonoPhrase -> IO ()
writeAsMIDI path = MAIN.writeAsMIDI path . translateToMain



outputAsTabular :: (Pretty pch, Pretty drn) 
                => GlobalRenderInfo -> Phrase pch drn anno -> String
outputAsTabular _gi ph = ppRender $ monoTabular lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsTabular :: (Pretty pch, Pretty drn) 
               => GlobalRenderInfo -> Phrase pch drn anno ->  IO ()
printAsTabular gi = putStrLn . outputAsTabular gi




outputAsLinear :: (Pretty pch, Pretty drn) 
                => GlobalRenderInfo -> Phrase pch drn anno -> String
outputAsLinear _gi ph = ppRender $ monoLinear lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsLinear :: (Pretty pch, Pretty drn) 
               => GlobalRenderInfo -> Phrase pch drn anno ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi
