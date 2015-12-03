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
  , default_score_info

  , SectionInfo(..)
  , UnitNoteLength(..)
  , default_section_info


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


import qualified Payasan.Base.Elementary.Internal.ElementaryToMain    as ELEM
import qualified Payasan.Base.Elementary.Internal.Syntax              as ELEM
import qualified Payasan.Base.Elementary.Internal.Traversals          as ELEM
import Payasan.Base.Elementary.Internal.LinearOutput
import Payasan.Base.Elementary.Internal.TabularOutput
import qualified Payasan.Base.Elementary.Notelist                     as ELEM

import Payasan.Base.Internal.AddBeams
import Payasan.Base.Internal.CommonSyntax
import qualified Payasan.Base.Internal.MainSyntax as MAIN
import Payasan.Base.Internal.Output.Common ( LeafOutput(..) )
import Payasan.Base.Internal.Shell

import qualified Payasan.Base.Internal.LilyPond.OutTrans        as LY
import qualified Payasan.Base.Internal.LilyPond.RhythmicMarkup  as RHY
import Payasan.Base.Internal.LilyPond.Utils

import qualified Payasan.Base.Notelist as MAIN
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty

-- Use Elementary syntax...

fromLilyPond :: LyChordPhrase -> StdChordPhrase
fromLilyPond = fromLilyPondWith default_section_info

fromLilyPondWith :: SectionInfo 
                 -> LyChordPhrase 
                 -> StdChordPhrase
fromLilyPondWith locals = translateInput . ELEM.pushSectionInfo locals



outputAsLilyPond :: ScoreInfo -> StdChordPhrase -> String
outputAsLilyPond globals = 
    MAIN.ppRender . MAIN.genOutputAsLilyPond config
                  . ELEM.translateToMain 
                  . translateOutput
  where
    config  = MAIN.LilyPondPipeline 
                { MAIN.beam_trafo  = addBeams
                , MAIN.out_trafo   = LY.translateToOutput_DurationOnly
                , MAIN.output_func = chordmodeOutput globals 
                }


printAsLilyPond :: ScoreInfo -> StdChordPhrase -> IO ()
printAsLilyPond globals = putStrLn . outputAsLilyPond globals


outputAsRhythmicMarkup :: ScoreInfo -> StdChordPhrase -> String
outputAsRhythmicMarkup globals = 
    MAIN.ppRender . ELEM.genOutputAsRhythmicMarkup def globals
  where
    def = RHY.MarkupOutput { RHY.asMarkup = \p -> tiny_ (braces $ pPrint p) }


printAsRhythmicMarkup :: ScoreInfo -> StdChordPhrase -> IO ()
printAsRhythmicMarkup globals = putStrLn . outputAsRhythmicMarkup globals


ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdChordPhrase -> IO ()
writeAsMIDI path notes = MAIN.writeAsMIDI path $ midiTrans notes 


-- midiTrans :: StdChordPhrase -> MAIN.
-- The ELEM to MAIN translation is actually shape changing 
-- for MIDI - notes become chords...

midiTrans :: StdChordPhrase -> MAIN.Phrase Pitch Duration ()
midiTrans = ELEM.chordTranslateToMain . chordTrans



chordTrans :: StdChordPhrase -> ELEM.Phrase [Pitch] Duration ()
chordTrans = ELEM.mapPitch buildNotes


outputAsTabular :: ScoreInfo -> StdChordPhrase -> String
outputAsTabular _gi ph = ppRender $ elemTabular lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsTabular :: ScoreInfo -> StdChordPhrase ->  IO ()
printAsTabular gi = putStrLn . outputAsTabular gi




outputAsLinear :: ScoreInfo -> StdChordPhrase -> String
outputAsLinear _gi ph = ppRender $ elemLinear lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsLinear :: ScoreInfo -> StdChordPhrase ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi
