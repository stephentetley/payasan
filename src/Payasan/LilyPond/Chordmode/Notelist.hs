{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Chordmode.Notelist
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

module Payasan.LilyPond.Chordmode.Notelist
  ( 

    module Payasan.Base.Internal.Shell

  , StdChordPhrase
  , chordmode

  , ScoreInfo(..)
  , OctaveMode(..)
  , default_score_info

  , LocalContextInfo(..)
  , UnitNoteLength(..)
  , default_local_info


  , fromLilyPond
  , fromLilyPondWith

  , outputAsLilyPond
  , printAsLilyPond

  , outputAsRhythmicMarkup
  , printAsRhythmicMarkup

  , ppRender

  , writeAsMIDI

  , outputAsTabular
  , printAsTabular

  , outputAsLinear
  , printAsLinear


  ) where

import Payasan.LilyPond.Chordmode.Internal.Base
import Payasan.LilyPond.Chordmode.Internal.InTrans
import Payasan.LilyPond.Chordmode.Internal.Interpret
import Payasan.LilyPond.Chordmode.Internal.Output
import Payasan.LilyPond.Chordmode.Internal.OutTrans
import Payasan.LilyPond.Chordmode.Internal.Parser (chordmode)  -- to re-export


import qualified Payasan.Base.Monophonic.Internal.MonoToMain    as MONO
import qualified Payasan.Base.Monophonic.Internal.Syntax        as MONO
import qualified Payasan.Base.Monophonic.Internal.Traversals    as MONO
import Payasan.Base.Monophonic.Internal.LinearOutput
import Payasan.Base.Monophonic.Internal.TabularOutput
import qualified Payasan.Base.Monophonic.Notelist               as MONO

import Payasan.Base.Internal.AddBeams
import Payasan.Base.Internal.CommonSyntax
import qualified Payasan.Base.Internal.MainSyntax as MAIN
import Payasan.Base.Internal.MainToBeam
import Payasan.Base.Internal.Output.Common ( LeafOutput(..) )
import Payasan.Base.Internal.Shell

import qualified Payasan.Base.Internal.LilyPond.OutTrans        as LY
import qualified Payasan.Base.Internal.LilyPond.RhythmicMarkup  as RHY
import Payasan.Base.Internal.LilyPond.Utils

import qualified Payasan.Base.Notelist as MAIN
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty

-- Use Monophonic syntax...

fromLilyPond :: ScoreInfo -> LyChordPhrase -> StdChordPhrase
fromLilyPond gi = fromLilyPondWith gi default_local_info

fromLilyPondWith :: ScoreInfo 
                 -> LocalContextInfo 
                 -> LyChordPhrase 
                 -> StdChordPhrase
fromLilyPondWith _gi ri = 
    translateInput . MONO.pushContextInfo ri



outputAsLilyPond :: ScoreInfo -> StdChordPhrase -> String
outputAsLilyPond gi =
    ppRender . chordmodeOutput gi 
             . LY.translateToOutput_DurationOnly
             . addBeams 
             . translateToBeam 
             . MONO.translateToMain 
             . translateOutput



printAsLilyPond :: ScoreInfo -> StdChordPhrase -> IO ()
printAsLilyPond gi = putStrLn . outputAsLilyPond gi


outputAsRhythmicMarkup :: ScoreInfo -> StdChordPhrase -> String
outputAsRhythmicMarkup gi = MONO.genOutputAsRhythmicMarkup def gi
  where
    def = RHY.MarkupOutput { RHY.asMarkup = \p -> tiny (braces $ pPrint p) }


printAsRhythmicMarkup :: ScoreInfo -> StdChordPhrase -> IO ()
printAsRhythmicMarkup gi = putStrLn . outputAsRhythmicMarkup gi


ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdChordPhrase -> IO ()
writeAsMIDI path notes = MAIN.writeAsMIDI path $ midiTrans notes 


-- midiTrans :: StdChordPhrase -> MAIN.
-- The MONO to MAIN translation is actually shape changing 
-- for MIDI - notes become chords...

midiTrans :: StdChordPhrase -> MAIN.Phrase Pitch Duration ()
midiTrans = MONO.chordTranslateToMain . chordTrans



chordTrans :: StdChordPhrase -> MONO.Phrase [Pitch] Duration ()
chordTrans = MONO.mapPitch buildNotes


outputAsTabular :: ScoreInfo -> StdChordPhrase -> String
outputAsTabular _gi ph = ppRender $ monoTabular lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsTabular :: ScoreInfo -> StdChordPhrase ->  IO ()
printAsTabular gi = putStrLn . outputAsTabular gi




outputAsLinear :: ScoreInfo -> StdChordPhrase -> String
outputAsLinear _gi ph = ppRender $ monoLinear lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsLinear :: ScoreInfo -> StdChordPhrase ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi
