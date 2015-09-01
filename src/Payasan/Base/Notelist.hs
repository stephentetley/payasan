{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Notelist
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

module Payasan.Base.Notelist
  ( 

    StdPhrase

  , ABCPhrase           -- * re-export
  , abc                 -- * re-export

  , LyPhrase
  , lilypond

  , GlobalRenderInfo(..)
  , PitchDirective(..)

  , LocalRenderInfo(..)
  , UnitNoteLength(..)
  , default_local_info


  , fromABC
  , fromABCWith
  , fromLilyPond
  , fromLilyPondWith
  
  , outputAsABC
  , printAsABC

  , ppRender

  , writeAsMIDI

  ) where

import qualified Payasan.Base.Internal.ABC.InTrans          as ABCIn
import qualified Payasan.Base.Internal.ABC.OutTrans         as ABCOut
import Payasan.Base.Internal.ABC.Output
import Payasan.Base.Internal.ABC.Parser (abc)
import Payasan.Base.Internal.ABC.Syntax (ABCPhrase)

import qualified Payasan.Base.Internal.LilyPond.InTrans     as LYIn
import Payasan.Base.Internal.LilyPond.Parser (lilypond)
import Payasan.Base.Internal.LilyPond.Syntax (LyPhrase)

import qualified Payasan.Base.Internal.MIDI.Output          as MIDI
import qualified Payasan.Base.Internal.MIDI.OutTrans        as MIDIOut
import qualified Payasan.Base.Internal.MIDI.PitchTrans      as MIDIPch
import qualified Payasan.Base.Internal.MIDI.Syntax          as MIDI


import qualified Payasan.Base.Internal.BracketTrans         as BRKT
import Payasan.Base.Internal.MainSyntax

import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJ                -- package: pretty

type StdPhrase = Phrase Pitch Duration



fromABC :: ABCPhrase -> StdPhrase
fromABC = fromABCWith default_local_info

fromABCWith :: LocalRenderInfo -> ABCPhrase -> StdPhrase
fromABCWith ri = ABCIn.translate . ABCIn.pushLocalRenderInfo ri

fromLilyPond :: GlobalRenderInfo -> LyPhrase -> StdPhrase
fromLilyPond gi = fromLilyPondWith gi default_local_info

fromLilyPondWith :: GlobalRenderInfo -> LocalRenderInfo -> LyPhrase -> StdPhrase
fromLilyPondWith gi ri = LYIn.translate gi . LYIn.pushLocalRenderInfo ri



outputAsABC :: StdPhrase -> String
outputAsABC = ppRender . abcOutput . ABCOut.translate . BRKT.transAndBeam id

printAsABC :: StdPhrase -> IO ()
printAsABC = putStrLn . outputAsABC


ppRender :: Doc -> String
ppRender = renderStyle (style {lineLength=500})


writeAsMIDI :: FilePath -> StdPhrase -> IO ()
writeAsMIDI path notes = 
   let trk = MIDIOut.translate (MIDI.simpleTrackData 1) (noteTrans notes)
   in MIDI.writeMF1 path [trk]

noteTrans :: StdPhrase -> Phrase MIDI.MidiPitch Duration
noteTrans = MIDIPch.translate 