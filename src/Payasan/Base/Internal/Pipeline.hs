{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Pipeline
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

module Payasan.Base.Internal.Pipeline
  ( 

    StdPhrase

  , ABCPhrase           -- * re-export
  , abc                 -- * re-export

  , LyPhrase
  , lilypond

  , GlobalRenderInfo(..)
  , OctaveMode(..)
  , default_global_info 

  , LocalRenderInfo(..)
  , UnitNoteLength(..)
  , default_local_info


  , fromABC
  , fromABCWith
  , fromABCWithIO       -- temp ?
  , fromLilyPond
  , fromLilyPondWith
  , fromLilyPondWithIO  -- temp ?
  
  , outputAsABC
  , printAsABC
  , outputAsLilyPond
  , printAsLilyPond

  , ppRender

  , writeAsMIDI

  ) where

import qualified Payasan.Base.Internal.ABC.InTrans          as ABCIn
import qualified Payasan.Base.Internal.ABC.OutTrans         as ABCOut
import Payasan.Base.Internal.ABC.Output (abcOutput)
import Payasan.Base.Internal.ABC.Parser (abc)
import Payasan.Base.Internal.ABC.Syntax (ABCPhrase)

import qualified Payasan.Base.Internal.LilyPond.InTrans     as LYIn
import qualified Payasan.Base.Internal.LilyPond.OutTrans    as LYOut
import Payasan.Base.Internal.LilyPond.Output (lilyPondOutput, LyOutputDef(..))
import Payasan.Base.Internal.LilyPond.Quasiquote (lilypond)
import Payasan.Base.Internal.LilyPond.Syntax (LyPhrase)
import Payasan.Base.Internal.LilyPond.Utils (pitch)

import qualified Payasan.Base.Internal.MIDI.Output          as MIDI
import qualified Payasan.Base.Internal.MIDI.RenderOutput    as MIDI
import qualified Payasan.Base.Internal.MIDI.PitchTrans      as MIDI
import qualified Payasan.Base.Internal.MIDI.PrimitiveSyntax as MIDI


import Payasan.Base.Internal.AddBeams
import qualified Payasan.Base.Internal.BeamSyntax           as BEAM
import Payasan.Base.Internal.BeamToMain
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.MainToBeam
import Payasan.Base.Internal.MainSyntax

import Payasan.Base.Internal.Tabular.Common
import Payasan.Base.Internal.Tabular.OutputBeam
import Payasan.Base.Internal.Tabular.OutputMain



import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJ                -- package: pretty

type StdPhrase = Phrase Pitch Duration () 


--------------------------------------------------------------------------------
-- Writer monad for debugging / tracing

(<||>) :: Doc -> Doc -> Doc
a <||> b = a $+$ text "" $+$ b

-- | Writer monad to collect debug output, concat is (<||>).
--
newtype W a = W { getW :: (Doc,a) }

instance Functor W where
  fmap f ma = W $ let (w,a) = getW ma in (w,f a)

instance Applicative W where
  pure a    = W (empty, a)
  mf <*> ma = W $ let (w1,f) = getW mf
                      (w2,a) = getW ma
                  in (w1 <||> w2, f a)

instance Monad W where
  return    = pure
  ma >>= k  = W $ let (w1,a) = getW ma 
                      (w2,b) = getW (k a)
                  in (w1 <||> w2, b)
            
runW :: W a -> (Doc,a) 
runW = getW

tell :: Doc -> W ()
tell d = W $ (d,())                
  
debug :: (a -> Doc) -> a -> W a
debug f a = tell (f a) >> return a

--------------------------------------------------------------------------------
-- 

fromABC :: ABCPhrase -> StdPhrase
fromABC = fromABCWith default_local_info

fromABCWith :: LocalRenderInfo -> ABCPhrase -> StdPhrase
fromABCWith ri = translateToMain . ABCIn.translate . BEAM.pushLocalRenderInfo ri


fromABCWithIO :: LocalRenderInfo -> ABCPhrase -> IO StdPhrase
fromABCWithIO ri ph = 
    let (out,a) = runW body in do { putStrLn (ppRender out); return a }
  where
    body = do { ph1 <- debug (beamTabular std_abc_output) $ BEAM.pushLocalRenderInfo ri ph
              ; ph2 <- debug (beamTabular pitch_duration_output) $ ABCIn.translate ph1
              ; ph3 <- debug (mainTabular pitch_duration_output) $ translateToMain ph2
              ; return ph3
              }



fromLilyPond :: GlobalRenderInfo -> LyPhrase () -> StdPhrase 
fromLilyPond gi = fromLilyPondWith gi default_local_info


fromLilyPondWith :: GlobalRenderInfo -> LocalRenderInfo -> LyPhrase () -> StdPhrase
fromLilyPondWith gi ri = 
    translateToMain . LYIn.translate gi . BEAM.pushLocalRenderInfo ri

fromLilyPondWithIO :: GlobalRenderInfo 
                   -> LocalRenderInfo 
                   -> LyPhrase () 
                   -> IO StdPhrase
fromLilyPondWithIO gi ri ph = 
    let (out,a) = runW body in do { putStrLn (ppRender out); return a }
  where
    body = do { ph1 <- debug (beamTabular std_ly_output) $ BEAM.pushLocalRenderInfo ri ph
              ; ph2 <- debug (beamTabular pitch_duration_output) $ LYIn.translate gi ph1
              ; ph3 <- debug (mainTabular pitch_duration_output) $ translateToMain ph2
              ; return ph3
              }



outputAsABC :: GlobalRenderInfo -> StdPhrase -> String
outputAsABC gi = ppRender . abcOutput gi . ABCOut.translate . addBeams . translateToBeam

printAsABC :: GlobalRenderInfo -> StdPhrase -> IO ()
printAsABC gi = putStrLn . outputAsABC gi

outputAsLilyPond :: GlobalRenderInfo -> StdPhrase -> String
outputAsLilyPond gi = 
    ppRender . lilyPondOutput gi std_def 
             . LYOut.translate gi 
             . addBeams 
             . translateToBeam
  where
    std_def = LyOutputDef { printPitch = pitch, printAnno = \_ -> empty }

printAsLilyPond :: GlobalRenderInfo -> StdPhrase -> IO ()
printAsLilyPond gi = putStrLn . outputAsLilyPond gi


ppRender :: Doc -> String
ppRender = renderStyle (style {lineLength=500})


writeAsMIDI :: FilePath -> StdPhrase -> IO ()
writeAsMIDI path notes = 
   let trk = MIDI.midiOutput (MIDI.simpleTrackData 1) (noteTrans notes)
   in MIDI.writeMF1 path [trk]

noteTrans :: StdPhrase -> BEAM.Phrase MIDI.MidiPitch Duration ()
noteTrans = MIDI.translate . translateToBeam