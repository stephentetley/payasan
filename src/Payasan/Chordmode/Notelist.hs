{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Chordmode.Notelist
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

module Payasan.Chordmode.Notelist
  ( 

    module Payasan.Base.Internal.Shell

  , StdChordPhrase
  , chordmode

  , GlobalRenderInfo(..)
  , OctaveMode(..)
  , default_global_info

  , LocalRenderInfo(..)
  , UnitNoteLength(..)
  , default_local_info


  , fromLilyPond
  , fromLilyPondWith

  , outputAsLilyPond
  , printAsLilyPond

  , ppRender

--  , writeAsMIDI


  ) where

import Payasan.Chordmode.Internal.Base
import Payasan.Chordmode.Internal.InTrans
import Payasan.Chordmode.Internal.Output
import Payasan.Chordmode.Internal.OutTrans
import Payasan.Chordmode.Internal.Parser (chordmode)  -- to re-export


import qualified Payasan.Base.Monophonic.Internal.MonoToMain as MONO
import qualified Payasan.Base.Monophonic.Internal.Syntax     as MONO

import Payasan.Base.Internal.AddBeams
import Payasan.Base.Internal.CommonSyntax
import qualified Payasan.Base.Internal.MainSyntax as MAIN
import Payasan.Base.Internal.MainToBeam
import Payasan.Base.Internal.Shell

import qualified Payasan.Base.Internal.LilyPond.OutTrans    as LYOut
import qualified Payasan.Base.Internal.LilyPond.Syntax      as LY

-- import qualified Payasan.Base.Internal.MIDI.Output          as MIDI
-- import qualified Payasan.Base.Internal.MIDI.RenderOutput    as MIDI
-- import qualified Payasan.Base.Internal.MIDI.PrimitiveSyntax as MIDI

import qualified Payasan.Base.Notelist as MAIN
import Payasan.Base.Duration

import Text.PrettyPrint.HughesPJ        -- package: pretty

-- Use Monophonic syntax...

fromLilyPond :: GlobalRenderInfo -> LyChordPhrase -> StdChordPhrase
fromLilyPond gi = fromLilyPondWith gi default_local_info

fromLilyPondWith :: GlobalRenderInfo 
                 -> LocalRenderInfo 
                 -> LyChordPhrase 
                 -> StdChordPhrase
fromLilyPondWith _gi ri = 
    translateInput . MONO.pushLocalRenderInfo ri



outputAsLilyPond :: GlobalRenderInfo -> StdChordPhrase -> String
outputAsLilyPond gi =
    ppRender . chordmodeOutput gi 
             . LYOut.translateDurationOnly
             . addBeams 
             . translateToBeam 
             . toMain

toMain ::  StdChordPhrase -> MAIN.Phrase LY.Pitch Duration ChordSuffix
toMain = MONO.translateToMain . translateOutput

printAsLilyPond :: GlobalRenderInfo -> StdChordPhrase -> IO ()
printAsLilyPond gi = putStrLn . outputAsLilyPond gi


ppRender :: Doc -> String
ppRender = MAIN.ppRender

{-
writeAsMIDI :: FilePath -> StdDrumPhrase -> IO ()
writeAsMIDI path notes = 
   let trk = MIDI.midiOutput (MIDI.simpleTrackData 9) (noteTrans notes)
   in MIDI.writeMF1 path [trk]

noteTrans :: StdDrumPhrase -> BEAM.Phrase MIDI.MidiPitch Duration
noteTrans = PERC.translate . translateToBeam


-}