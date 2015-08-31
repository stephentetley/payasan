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

  , GlobalRenderInfo(..)
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

import qualified Payasan.Base.Internal.ABCInTrans       as ABCIn
import qualified Payasan.Base.Internal.ABCOutTrans      as ABCOut
import Payasan.Base.Internal.ABCOutput
import Payasan.Base.Internal.ABCParser (abc)
import Payasan.Base.Internal.ABCSyntax (ABCPhrase)
import qualified Payasan.Base.Internal.BracketTrans     as BRKT
import qualified Payasan.Base.Internal.LilyPondInTrans  as LYIn
import Payasan.Base.Internal.LilyPondSyntax (LyPhrase)
import Payasan.Base.Internal.MainSyntax
import qualified Payasan.Base.Internal.MidiOutput       as MIDI
import qualified Payasan.Base.Internal.MidiOutTrans     as MIDIOut
import qualified Payasan.Base.Internal.MidiPitchTrans   as MIDIPch
import qualified Payasan.Base.Internal.MidiSyntax       as MIDI

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