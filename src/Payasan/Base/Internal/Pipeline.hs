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
  , StdPhraseAnno

  , ABCPhrase           -- * re-export
  , abc                 -- * re-export

  , LyPhrase
  , lilypond

  , ScoreInfo(..)
  , OctaveMode(..)
  , default_score_info 

  , LocalContextInfo(..)
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

  , LilyPondPipeline(..)
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

  ) where

import qualified Payasan.Base.Internal.ABC.InTrans          as ABC
import qualified Payasan.Base.Internal.ABC.OutTrans         as ABC
import Payasan.Base.Internal.ABC.Output (abcOutput)
import Payasan.Base.Internal.ABC.Parser (abc)
import Payasan.Base.Internal.ABC.Syntax (ABCPhrase)

import qualified Payasan.Base.Internal.LilyPond.InTrans         as LY
import qualified Payasan.Base.Internal.LilyPond.RhythmicMarkup  as LY
import qualified Payasan.Base.Internal.LilyPond.OutTrans        as LY
import qualified Payasan.Base.Internal.LilyPond.SimpleOutput    as LY
import Payasan.Base.Internal.LilyPond.Quasiquote (lilypond)
import qualified Payasan.Base.Internal.LilyPond.Syntax          as LY
import Payasan.Base.Internal.LilyPond.Syntax (LyPhrase)
import Payasan.Base.Internal.LilyPond.Utils

import qualified Payasan.Base.Internal.MIDI.Output          as MIDI
import qualified Payasan.Base.Internal.MIDI.RenderOutput    as MIDI
import qualified Payasan.Base.Internal.MIDI.PitchTrans      as MIDI
import qualified Payasan.Base.Internal.MIDI.PrimitiveSyntax as MIDI

import Payasan.Base.Internal.Output.Common
import Payasan.Base.Internal.Output.Tabular.OutputBeam
import Payasan.Base.Internal.Output.Tabular.OutputMain
import Payasan.Base.Internal.Output.Linear.OutputMain


import Payasan.Base.Internal.AddBeams
import Payasan.Base.Internal.Base
import qualified Payasan.Base.Internal.BeamSyntax           as BEAM
import Payasan.Base.Internal.BeamToMain
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.MainToBeam
import Payasan.Base.Internal.MainSyntax




import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty



type StdPhrase          = Phrase Pitch Duration () 
type StdPhraseAnno anno = Phrase Pitch Duration anno


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

fromABCWith :: LocalContextInfo -> ABCPhrase -> StdPhrase
fromABCWith locals = translateToMain . ABC.translateFromInput . BEAM.pushContextInfo locals


fromABCWithIO :: LocalContextInfo -> ABCPhrase -> IO StdPhrase
fromABCWithIO locals ph = 
    let (out,a) = runW body in do { putStrLn (ppRender out); return a }
  where
    body = do { ph1 <- debug (beamTabular std_abc_output) $ BEAM.pushContextInfo locals ph
              ; ph2 <- debug (beamTabular pitch_duration_output) $ ABC.translateFromInput ph1
              ; ph3 <- debug (mainTabular pitch_duration_output) $ translateToMain ph2
              ; return ph3
              }



fromLilyPond :: ScoreInfo -> LY.LyPhrase () -> StdPhrase 
fromLilyPond gi = fromLilyPondWith gi default_local_info


fromLilyPondWith :: ScoreInfo -> LocalContextInfo -> LY.LyPhrase () -> StdPhrase
fromLilyPondWith gi ri = 
    translateToMain . LY.translateFromInput gi . BEAM.pushContextInfo ri

fromLilyPondWithIO :: ScoreInfo 
                   -> LocalContextInfo 
                   -> LY.LyPhrase () 
                   -> IO StdPhrase
fromLilyPondWithIO gi ri ph = 
    let (out,a) = runW body in do { putStrLn (ppRender out); return a }
  where
    body = do { ph1 <- debug (beamTabular std_ly_output) $ BEAM.pushContextInfo ri ph
              ; ph2 <- debug (beamTabular pitch_duration_output) $ LY.translateFromInput gi ph1
              ; ph3 <- debug (mainTabular pitch_duration_output) $ translateToMain ph2
              ; return ph3
              }



outputAsABC :: ScoreInfo -> StdPhrase -> String
outputAsABC info = 
    ppRender . abcOutput info 
             . ABC.translateToOutput
             . addBeams 
             . translateToBeam

printAsABC :: ScoreInfo -> StdPhrase -> IO ()
printAsABC info = putStrLn . outputAsABC info



data LilyPondPipeline p1 a1 p2 a2 = LilyPondPipeline
    { beam_trafo    :: BEAM.Phrase p1 Duration a1 -> BEAM.Phrase p1 Duration a1
    , out_trafo     :: BEAM.Phrase p1 Duration a1 -> LY.GenLyPhrase p2 a2 
    , output_func   :: LY.GenLyPhrase p2 a2 -> Doc
    }


genOutputAsLilyPond :: LilyPondPipeline p1 a1 p2 a2
                    -> Phrase p1 Duration a1
                    -> String
genOutputAsLilyPond config = 
    ppRender . outputStep  
             . toGenLyPhrase 
             . beamingRewrite  
             . translateToBeam
  where
    outputStep          = output_func config
    toGenLyPhrase       = out_trafo config
    beamingRewrite      = beam_trafo config


outputAsLilyPond :: Anno anno => ScoreInfo -> StdPhraseAnno anno -> String
outputAsLilyPond globals = genOutputAsLilyPond config
  where
    config  = LilyPondPipeline { beam_trafo  = addBeams
                               , out_trafo   = LY.translateToOutput globals
                               , output_func = LY.simpleLyOutput std_def globals 
                               }
    std_def = LY.LyOutputDef { LY.printPitch = pitch, LY.printAnno = anno }


printAsLilyPond :: Anno anno => ScoreInfo -> StdPhraseAnno anno -> IO ()
printAsLilyPond gi = putStrLn . outputAsLilyPond gi



-- Rhythmic markup generally should be beamed.

genOutputAsRhythmicMarkup :: LY.MarkupOutput pch 
                          -> ScoreInfo
                          -> Phrase pch Duration anno 
                          -> String
genOutputAsRhythmicMarkup def info = 
    ppRender . LY.simpleLyOutput ppDef info
             . LY.translateToRhythmicMarkup def
             . addBeams 
             . translateToBeam
  where
    ppDef = LY.LyOutputDef { LY.printPitch = pitch, LY.printAnno = markup }


outputAsRhythmicMarkup :: ScoreInfo -> StdPhrase -> String
outputAsRhythmicMarkup gi = genOutputAsRhythmicMarkup def gi
  where
    def = LY.MarkupOutput { LY.asMarkup = \p -> tiny (braces $ pPrint p) }


printAsRhythmicMarkup :: ScoreInfo -> StdPhrase -> IO ()
printAsRhythmicMarkup gi = putStrLn . outputAsRhythmicMarkup gi



ppRender :: Doc -> String
ppRender = renderStyle (style {lineLength=500})


writeAsMIDI :: FilePath -> StdPhrase -> IO ()
writeAsMIDI path notes = 
    let trk = MIDI.midiOutput (MIDI.simpleTrackData 1) (noteTrans notes)
    in MIDI.writeMF1 path [trk]

noteTrans :: StdPhrase -> BEAM.Phrase MIDI.MidiPitch Duration ()
noteTrans = MIDI.translate . translateToBeam


outputAsTabular :: (Pretty pch, Pretty drn) 
                => ScoreInfo -> Phrase pch drn anno -> String
outputAsTabular _gi ph = ppRender $ mainTabular lo ph
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
outputAsLinear _gi ph = ppRender $ mainLinear lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsLinear :: (Pretty pch, Pretty drn) 
              => ScoreInfo -> Phrase pch drn anno ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi
