{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Pipeline
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

module Payasan.PSC.Pipeline
  ( 

    StdPart             -- * re-export
  
  , ABCPart             -- * re-export
  , abc                 -- * re-export

  , LyPart1             -- * re-export
  , lilypond

  , ScoreInfo(..)
  , default_score_info 

  , StaffInfo(..)
  , default_staff_info 

  , SectionInfo(..)
  , UnitNoteLength(..)
  , default_section_info


  , fromABC
  , fromABCWith
  , fromABCWithIO       -- temp ?

  , fromLilyPond_Relative
  , fromLilyPondWith_Relative
  , fromLilyPondWithIO_Relative  -- temp ?
  
  , outputAsABC
  , printAsABC

  , LilyPondPipeline(..)
  , genOutputAsLilyPond

  , LilyPondPipeline2(..)
  , genOutputAsLilyPond2


  , outputAsLilyPond_Relative
  , printAsLilyPond_Relative

  , genOutputAsRhythmicMarkup
  , outputAsRhythmicMarkup
  , printAsRhythmicMarkup


  , ppRender

  , writeAsMIDI

  , outputAsCsound
  , printAsCsound

  , outputAsTabular
  , printAsTabular

  , outputAsLinear
  , printAsLinear

  , beamAsTabular
  , beamAsLinear

  ) where

-- Note - temporary imports
-- Csound imports should not be accessed by Payasan.Base
import Payasan.PSC.Backend.Csound.BeamToCsound
import Payasan.PSC.Backend.Csound.Output
import qualified Payasan.PSC.Backend.Csound.OutTrans            as CS

import qualified Payasan.PSC.Backend.MIDI.BeamToMIDI            as MIDI
import qualified Payasan.PSC.Backend.MIDI.Output                as MIDI
import qualified Payasan.PSC.Backend.MIDI.OutTrans              as MIDI
import qualified Payasan.PSC.Backend.MIDI.PrimitiveSyntax       as MIDI


import qualified Payasan.PSC.Backend.ABC.InTrans              as ABC
import qualified Payasan.PSC.Backend.ABC.OutTrans             as ABC
import Payasan.PSC.Backend.ABC.Output (abcOutput)
import Payasan.PSC.Repr.External.ABCParser (abc)
import Payasan.PSC.Backend.ABC.Syntax (ABCPart)


import qualified Payasan.PSC.Backend.LilyPond.InTrans         as LY
import qualified Payasan.PSC.Backend.LilyPond.RhythmicMarkup  as LY
import qualified Payasan.PSC.Backend.LilyPond.OutTrans        as LY
import Payasan.PSC.Repr.External.LilyPondParser (lilypond)
import qualified Payasan.PSC.Backend.LilyPond.SimpleOutput    as LY
import qualified Payasan.PSC.Backend.LilyPond.Syntax          as LY
import Payasan.PSC.Backend.LilyPond.Syntax (LyPart1)
import Payasan.PSC.Backend.LilyPond.Utils


import Payasan.PSC.Backend.Output.Common
import Payasan.PSC.Backend.Output.Tabular.OutputBeam
import Payasan.PSC.Backend.Output.Tabular.OutputMain
import Payasan.PSC.Backend.Output.Linear.OutputBeam
import Payasan.PSC.Backend.Output.Linear.OutputMain

import Payasan.PSC.Base.SyntaxCommon

import Payasan.PSC.Repr.IRBeamToExternal
import Payasan.PSC.Repr.ExternalToIRBeam
import Payasan.PSC.Repr.External.Syntax

import Payasan.PSC.Repr.IRBeam.AddBeams
import qualified Payasan.PSC.Repr.IRBeam.Syntax               as BEAM




import Payasan.Base.Duration
import Payasan.Base.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty





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

fromABC :: ABCPart -> StdPart
fromABC = fromABCWith default_section_info

fromABCWith :: SectionInfo -> ABCPart -> StdPart
fromABCWith locals = 
    translateToMain . ABC.translateFromInput . BEAM.pushSectionInfo locals


fromABCWithIO :: SectionInfo -> ABCPart -> IO StdPart
fromABCWithIO locals ph = 
    let (out,a) = runW body in do { putStrLn (ppRender out); return a }
  where
    body = do { ph1 <- debug (beamTabular std_abc_output) $ BEAM.pushSectionInfo locals ph
              ; ph2 <- debug (beamTabular pitch_duration_output) $ ABC.translateFromInput ph1
              ; ph3 <- debug (mainTabular pitch_duration_output) $ translateToMain ph2
              ; return ph3
              }



fromLilyPond_Relative :: Pitch -> LY.LyPart1 () -> StdPart 
fromLilyPond_Relative pch = fromLilyPondWith_Relative pch default_section_info


fromLilyPondWith_Relative :: Pitch -> SectionInfo -> LY.LyPart1 () -> StdPart
fromLilyPondWith_Relative pch locals = 
    translateToMain . LY.translateFromInput_Relative pch . BEAM.pushSectionInfo locals


fromLilyPondWithIO_Relative :: Pitch
                            -> SectionInfo 
                            -> LY.LyPart1 () 
                            -> IO StdPart
fromLilyPondWithIO_Relative pch locals ph = 
    let (out,a) = runW body in do { putStrLn (ppRender out); return a }
  where
    body = do { ph1 <- debug (beamTabular std_ly_output) $ BEAM.pushSectionInfo locals ph
              ; ph2 <- debug (beamTabular pitch_duration_output) $ LY.translateFromInput_Relative pch ph1
              ; ph3 <- debug (mainTabular pitch_duration_output) $ translateToMain ph2
              ; return ph3
              }



outputAsABC :: ScoreInfo -> StaffInfo -> StdPart1 anno -> String
outputAsABC infos staff = 
    ppRender . abcOutput infos staff
             . ABC.translateToOutput
             . addBeams 
             . translateToBeam

printAsABC :: ScoreInfo -> StaffInfo -> StdPart1 anno -> IO ()
printAsABC infos staff = putStrLn . outputAsABC infos staff


-- | This can capture both full score output and just notelist 
-- output by supplying the appropriate output function.
--
-- Libraries should define two output functions when appropriate:
-- one for full score and one for just notelist.
--
data LilyPondPipeline p1i a1i p1o a1o = LilyPondPipeline
    { beam_trafo    :: BEAM.Part p1i Duration a1i -> BEAM.Part p1i Duration a1i
    , out_trafo     :: BEAM.Part p1i Duration a1i -> LY.LyPart2 p1o a1o
    , output_func   :: LY.LyPart2 p1o a1o -> Doc
    }




genOutputAsLilyPond :: LilyPondPipeline p1i a1i p1o a1o
                    -> Part p1i Duration a1i
                    -> Doc
genOutputAsLilyPond config = 
    outputStep . toGenLyPart . beamingRewrite . translateToBeam
  where
    beamingRewrite      = beam_trafo config
    toGenLyPart       = out_trafo config
    outputStep          = output_func config


data LilyPondPipeline2 p1i a1i p2i a2i p1o a1o p2o a2o  = LilyPondPipeline2
    { pipe2_beam_trafo1   :: BEAM.Part p1i Duration a1i -> BEAM.Part p1i Duration a1i
    , pipe2_out_trafo1    :: BEAM.Part p1i Duration a1i -> LY.LyPart2 p1o a1o
    , pipe2_beam_trafo2   :: BEAM.Part p2i Duration a2i -> BEAM.Part p2i Duration a2i
    , pipe2_out_trafo2    :: BEAM.Part p2i Duration a2i -> LY.LyPart2 p2o a2o
    , pipe2_output_func   :: LY.LyPart2 p1o a1o -> LY.LyPart2 p2o a2o -> Doc
    }



genOutputAsLilyPond2 :: LilyPondPipeline2 p1i a1i p2i a2i p1o a1o p2o a2o 
                     -> Part p1i Duration a1i
                     -> Part p2i Duration a2i
                     -> Doc
genOutputAsLilyPond2 config ph1 ph2 = 
    let a = toGenLyPart1 $ beamingRewrite1 $ translateToBeam ph1
        b = toGenLyPart2 $ beamingRewrite2 $ translateToBeam ph2
    in outputStep a b
  where
    beamingRewrite1     = pipe2_beam_trafo1 config
    toGenLyPart1      = pipe2_out_trafo1 config
    beamingRewrite2     = pipe2_beam_trafo2 config
    toGenLyPart2      = pipe2_out_trafo2 config
    outputStep          = pipe2_output_func config



outputAsLilyPond_Relative :: Anno anno 
                          => ScoreInfo -> Pitch -> StdPart1 anno -> String
outputAsLilyPond_Relative infos pch = ppRender . genOutputAsLilyPond config
  where
    config  = LilyPondPipeline { beam_trafo  = addBeams
                               , out_trafo   = LY.translateToOutput_Relative pch
                               , output_func = LY.simpleScore_Relative std_def infos pch
                               }
    std_def = LY.LyOutputDef { LY.printPitch = pitch, LY.printAnno = anno }


printAsLilyPond_Relative :: Anno anno 
                         => ScoreInfo -> Pitch -> StdPart1 anno -> IO ()
printAsLilyPond_Relative infos pch = putStrLn . outputAsLilyPond_Relative infos pch



-- Rhythmic markup generally should be beamed.

genOutputAsRhythmicMarkup :: LY.MarkupOutput pch 
                          -> ScoreInfo                          
                          -> Part pch Duration anno 
                          -> Doc
genOutputAsRhythmicMarkup def infos = 
    LY.rhythmicMarkupScore ppDef infos . LY.translateToRhythmicMarkup def
                                       . addBeams 
                                       . translateToBeam
  where
    ppDef = LY.LyOutputDef { LY.printPitch = pitch, LY.printAnno = const empty }


outputAsRhythmicMarkup :: ScoreInfo -> StdPart1 anno -> String
outputAsRhythmicMarkup infos = 
    ppRender . genOutputAsRhythmicMarkup def infos 
  where
    def = LY.MarkupOutput { LY.asMarkup = \p -> teeny_ (braces $ pPrint p) }


printAsRhythmicMarkup :: ScoreInfo -> StdPart -> IO ()
printAsRhythmicMarkup infos = putStrLn . outputAsRhythmicMarkup infos



ppRender :: Doc -> String
ppRender = renderStyle (style {lineLength=500})


--------------------------------------------------------------------------------
-- MIDI

-- Should we have a @genOutputAsMIDI@ function?


writeAsMIDI :: FilePath -> StdPart1 anno -> IO ()
writeAsMIDI path ph = 
    let notes   = MIDI.translateToMidiP $ translateToBeam ph
        trk     = MIDI.translateToMIDI (MIDI.simpleTrackData 1) notes
    in MIDI.writeMF1 path [trk]


--------------------------------------------------------------------------------
-- MIDI


outputAsCsound :: ColumnSpecs -> GenIStmt anno -> StdPart1 anno -> String
outputAsCsound cols gf ph =
    let notes   = CS.translateToCsoundP $ translateToBeam ph
        stmts   = translateToCsound gf notes
    in ppRender $ csoundOutput cols stmts

printAsCsound :: ColumnSpecs -> GenIStmt anno -> StdPart1 anno -> IO ()
printAsCsound cols gf = putStrLn . outputAsCsound cols gf


--------------------------------------------------------------------------------
-- Debug...

outputAsTabular :: (Pretty pch, Pretty drn) 
                => ScoreInfo -> Part pch drn anno -> String
outputAsTabular _gi ph = ppRender $ mainTabular lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsTabular :: (Pretty pch, Pretty drn) 
               => ScoreInfo -> Part pch drn anno ->  IO ()
printAsTabular gi = putStrLn . outputAsTabular gi


outputAsLinear :: (Pretty pch, Pretty drn) 
               => ScoreInfo -> Part pch drn anno -> String
outputAsLinear _gi ph = ppRender $ mainLinear lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

printAsLinear :: (Pretty pch, Pretty drn) 
              => ScoreInfo -> Part pch drn anno ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi



beamAsLinear :: (Pretty pch, Pretty drn) 
             => ScoreInfo -> BEAM.Part pch drn anno -> String
beamAsLinear _gi ph = ppRender $ beamLinear lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

beamAsTabular :: (Pretty pch, Pretty drn) 
              => ScoreInfo -> BEAM.Part pch drn anno -> String
beamAsTabular _gi ph = ppRender $ beamTabular lo ph
  where
    lo = LeafOutput { pp_pitch     = pPrint
                    , pp_duration  = pPrint
                    , pp_anno      = const empty
                    }

