{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.Chordmode.Notelist
-- Copyright   :  (c) Stephen Tetley 2015-2016
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

    module Payasan.PSC.Shell

  , StdChordSection
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


import qualified Payasan.Score.Elementary.Internal.ElementaryToExternal as ELEM
import qualified Payasan.Score.Elementary.Internal.Syntax               as ELEM
import qualified Payasan.Score.Elementary.Internal.Traversals           as ELEM
import qualified Payasan.Score.Elementary.Notelist                      as ELEM
import Payasan.Score.Elementary.Internal.ShowLinear
import Payasan.Score.Elementary.Internal.ShowTabular

import Payasan.PSC.Repr.External.AddBeams
import qualified Payasan.PSC.Repr.External.Syntax as MAIN

import Payasan.PSC.Base.ShowCommon ( LeafOutputNote(..) )
import Payasan.PSC.Shell
import Payasan.PSC.Base.SyntaxCommon

import qualified Payasan.PSC.LilyPond.OutTrans        as LY
import qualified Payasan.PSC.LilyPond.RhythmicMarkup  as RHY
import Payasan.PSC.LilyPond.Utils

import qualified Payasan.PSC.Pipeline                       as MAIN
import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty

-- Use Elementary syntax...

fromLilyPond :: LyChordSection -> StdChordSection
fromLilyPond = fromLilyPondWith default_section_info

fromLilyPondWith :: SectionInfo 
                 -> LyChordSection 
                 -> StdChordSection
fromLilyPondWith locals = translateInput . ELEM.pushSectionInfo locals



outputAsLilyPond :: ScoreInfo -> String -> StdChordSection -> String
outputAsLilyPond globals name = 
    MAIN.ppRender . MAIN.genOutputAsLilyPond config
                  . ELEM.transElementaryToExternal name
                  . translateOutput
  where
    config  = MAIN.LilyPondPipeline 
                { MAIN.beam_trafo  = addBeams
                , MAIN.out_trafo   = LY.translateToLyPartOut_DurationOnly
                , MAIN.output_func = chordmodeOutput globals 
                }


printAsLilyPond :: ScoreInfo -> String -> StdChordSection -> IO ()
printAsLilyPond globals name = putStrLn . outputAsLilyPond globals name


outputAsRhythmicMarkup :: ScoreInfo -> String -> StdChordSection -> String
outputAsRhythmicMarkup globals name = 
    MAIN.ppRender . ELEM.genOutputAsRhythmicMarkup def globals name
  where
    def = RHY.MarkupOutput { RHY.asMarkup = \p -> tiny_ (braces $ pPrint p) }


printAsRhythmicMarkup :: ScoreInfo -> String -> StdChordSection -> IO ()
printAsRhythmicMarkup globals name = 
    putStrLn . outputAsRhythmicMarkup globals name


ppRender :: Doc -> String
ppRender = MAIN.ppRender


writeAsMIDI :: FilePath -> StdChordSection -> IO ()
writeAsMIDI path notes = MAIN.writeAsMIDI path $ midiTrans notes 


-- midiTrans :: StdChordSection -> MAIN.
-- The ELEM to MAIN translation is actually shape changing 
-- for MIDI - notes become chords...

midiTrans :: StdChordSection -> MAIN.Part Pitch Duration ()
midiTrans = ELEM.chord_transElementaryToExternal "noname" . chordTrans



chordTrans :: StdChordSection -> ELEM.Section [Pitch] Duration ()
chordTrans = ELEM.mapPitch buildNotes


outputAsTabular :: ScoreInfo -> StdChordSection -> String
outputAsTabular _gi ph = ppRender $ elemTabular lo ph
  where
    lo = LeafOutputNote { pp_pitch     = pPrint
                        , pp_duration  = pPrint
                        , pp_anno      = const empty
                        }

printAsTabular :: ScoreInfo -> StdChordSection ->  IO ()
printAsTabular gi = putStrLn . outputAsTabular gi




outputAsLinear :: ScoreInfo -> StdChordSection -> String
outputAsLinear _gi ph = ppRender $ elemLinear lo ph
  where
    lo = LeafOutputNote { pp_pitch     = pPrint
                        , pp_duration  = pPrint
                        , pp_anno      = const empty
                        }

printAsLinear :: ScoreInfo -> StdChordSection ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi
