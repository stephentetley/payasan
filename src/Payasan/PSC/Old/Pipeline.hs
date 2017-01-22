{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Old.Pipeline
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- (Pipeline)
--
--------------------------------------------------------------------------------

module Payasan.PSC.Old.Pipeline
  ( 

    EXT.StdPart         -- * re-export
  
  , ABC.abc             -- * re-export

  , LY.lilypond        

  , ScoreInfo(..)
  , default_score_info 


  , SectionInfo(..)
  , UnitNoteLength(..)
  , default_section_info


  , fromABC
  , fromABCWith
  , fromABCWithIO       -- temp ?

  , fromLilyPond_Relative
  , fromLilyPondWith_Relative
  , fromLilyPondWithIO_Relative  -- temp ?
  
--  , outputAsABC
--  , printAsABC

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



  , outputAsTabular
  , printAsTabular

  , outputAsLinear
  , printAsLinear

  ) where







import qualified Payasan.PSC.Repr.External.ABCInTrans         as ABC
import qualified Payasan.PSC.Repr.External.ABCParser          as ABC



import qualified Payasan.PSC.Repr.External.LilyPondInTrans    as LY
import qualified Payasan.PSC.Repr.External.LilyPondParser     as LY

import qualified Payasan.PSC.LilyPond.RhythmicMarkup  as LYOut
import qualified Payasan.PSC.LilyPond.OutTrans        as LYOut
import qualified Payasan.PSC.LilyPond.SimpleOutput    as LYOut
import qualified Payasan.PSC.LilyPond.Utils           as LYOut


import Payasan.PSC.Base.ABCCommon
import Payasan.PSC.Base.LilyPondCommon
import Payasan.PSC.Base.ShowCommon
import Payasan.PSC.Repr.External.ShowLinear
import Payasan.PSC.Repr.External.ShowTabular

import Payasan.PSC.Base.SyntaxCommon

import qualified Payasan.PSC.Repr.External.Syntax             as EXT

import Payasan.PSC.Repr.External.AddBeams




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

transExternalToIRBeam :: a -> a
transExternalToIRBeam = id

fromABC :: EXT.Part ABCPitch ABCNoteLength () -> EXT.StdPart
fromABC = fromABCWith default_section_info

fromABCWith :: SectionInfo -> EXT.Part ABCPitch ABCNoteLength () -> EXT.StdPart
fromABCWith locals = 
    ABC.translateFromInput . EXT.pushSectionInfo locals


fromABCWithIO :: SectionInfo -> EXT.Part ABCPitch ABCNoteLength () -> IO EXT.StdPart
fromABCWithIO locals ph = 
    let (out,a) = runW body in do { putStrLn (ppRender out); return a }
  where
    body = do { ph1 <- debug (mainTabular std_abc_output) $ EXT.pushSectionInfo locals ph
              ; ph2 <- debug (mainTabular pitch_duration_output) $ ABC.translateFromInput ph1
              ; return ph2
              }



fromLilyPond_Relative :: Pitch -> EXT.Part LyPitch LyNoteLength () -> EXT.StdPart 
fromLilyPond_Relative pch = fromLilyPondWith_Relative pch default_section_info


fromLilyPondWith_Relative :: Pitch -> SectionInfo -> EXT.Part LyPitch LyNoteLength () -> EXT.StdPart
fromLilyPondWith_Relative pch locals = 
    LY.translateFromInput_Relative pch . EXT.pushSectionInfo locals


fromLilyPondWithIO_Relative :: Pitch
                            -> SectionInfo 
                            -> EXT.Part LyPitch LyNoteLength () 
                            -> IO EXT.StdPart
fromLilyPondWithIO_Relative pch locals ph = 
    let (out,a) = runW body in do { putStrLn (ppRender out); return a }
  where
    body = do { ph1 <- debug (mainTabular std_ly_output) $ EXT.pushSectionInfo locals ph
              ; ph2 <- debug (mainTabular pitch_duration_output) $ LY.translateFromInput_Relative pch ph1
              ; return ph2
              }


{-
outputAsABC :: ScoreInfo -> StaffInfo -> EXT.StdPart1 anno -> String
outputAsABC infos staff = 
    ppRender . ABCOut.abcOutput (score_title infos) (staff_clef staff) 4
             . ABCOut.translateToABCPartOut
             . addBeams 
             . transExternalToIRBeam

printAsABC :: ScoreInfo -> StaffInfo -> EXT.StdPart1 anno -> IO ()
printAsABC infos staff = putStrLn . outputAsABC infos staff
-}

-- | This can capture both full score output and just notelist 
-- output by supplying the appropriate output function.
--
-- Libraries should define two output functions when appropriate:
-- one for full score and one for just notelist.
--
data LilyPondPipeline p1i a1i p1o a1o = LilyPondPipeline
    { beam_trafo    :: EXT.Part p1i Duration a1i -> EXT.Part p1i Duration a1i
    , out_trafo     :: EXT.Part p1i Duration a1i -> EXT.Part p1o LyNoteLength a1o
    , output_func   :: EXT.Part p1o LyNoteLength a1o -> Doc
    }




genOutputAsLilyPond :: LilyPondPipeline p1i a1i p1o a1o
                    -> EXT.Part p1i Duration a1i
                    -> Doc
genOutputAsLilyPond config = 
    outputStep . toGenLyPart . beamingRewrite . transExternalToIRBeam
  where
    beamingRewrite      = beam_trafo config
    toGenLyPart       = out_trafo config
    outputStep          = output_func config


data LilyPondPipeline2 p1i a1i p2i a2i p1o a1o p2o a2o  = LilyPondPipeline2
    { pipe2_beam_trafo1   :: EXT.Part p1i Duration a1i -> EXT.Part p1i Duration a1i
    , pipe2_out_trafo1    :: EXT.Part p1i Duration a1i -> EXT.Part p1o LyNoteLength a1o
    , pipe2_beam_trafo2   :: EXT.Part p2i Duration a2i -> EXT.Part p2i Duration a2i
    , pipe2_out_trafo2    :: EXT.Part p2i Duration a2i -> EXT.Part p2o LyNoteLength a2o
    , pipe2_output_func   :: EXT.Part p1o LyNoteLength a1o -> EXT.Part p2o LyNoteLength a2o -> Doc
    }



genOutputAsLilyPond2 :: LilyPondPipeline2 p1i a1i p2i a2i p1o a1o p2o a2o 
                     -> EXT.Part p1i Duration a1i
                     -> EXT.Part p2i Duration a2i
                     -> Doc
genOutputAsLilyPond2 config ph1 ph2 = 
    let a = toGenLyPart1 $ beamingRewrite1 $ transExternalToIRBeam ph1
        b = toGenLyPart2 $ beamingRewrite2 $ transExternalToIRBeam ph2
    in outputStep a b
  where
    beamingRewrite1     = pipe2_beam_trafo1 config
    toGenLyPart1      = pipe2_out_trafo1 config
    beamingRewrite2     = pipe2_beam_trafo2 config
    toGenLyPart2      = pipe2_out_trafo2 config
    outputStep          = pipe2_output_func config



outputAsLilyPond_Relative :: Anno anno 
                          => ScoreInfo -> Pitch -> EXT.StdPart1 anno -> String
outputAsLilyPond_Relative infos pch = ppRender . genOutputAsLilyPond config
  where
    config  = LilyPondPipeline { beam_trafo  = addBeams
                               , out_trafo   = LYOut.translateToLyPartOut_Relative pch
                               , output_func = LYOut.simpleScore_Relative std_def infos pch
                               }
    std_def = LYOut.LyOutputDef { LYOut.printPitch = LYOut.pitch
                                , LYOut.printAnno = anno }


printAsLilyPond_Relative :: Anno anno 
                         => ScoreInfo -> Pitch -> EXT.StdPart1 anno -> IO ()
printAsLilyPond_Relative infos pch = putStrLn . outputAsLilyPond_Relative infos pch



-- Rhythmic markup generally should be beamed.

genOutputAsRhythmicMarkup :: LYOut.MarkupOutput pch 
                          -> ScoreInfo                          
                          -> EXT.Part pch Duration anno 
                          -> Doc
genOutputAsRhythmicMarkup def infos = 
    LYOut.rhythmicMarkupScore ppDef infos . LYOut.translateToLyPartOut_RhythmicMarkup def
                                          . addBeams 
                                          . transExternalToIRBeam
  where
    ppDef = LYOut.LyOutputDef { LYOut.printPitch = LYOut.pitch
                              , LYOut.printAnno = const empty }


outputAsRhythmicMarkup :: ScoreInfo -> EXT.StdPart1 anno -> String
outputAsRhythmicMarkup infos = 
    ppRender . genOutputAsRhythmicMarkup def infos 
  where
    def = LYOut.MarkupOutput { LYOut.asMarkup = \p -> LYOut.teeny_ (braces $ pPrint p) }


printAsRhythmicMarkup :: ScoreInfo -> EXT.StdPart -> IO ()
printAsRhythmicMarkup infos = putStrLn . outputAsRhythmicMarkup infos



ppRender :: Doc -> String
ppRender = renderStyle (style {lineLength=500})


--------------------------------------------------------------------------------
-- MIDI

-- Should we have a @genOutputAsMIDI@ function?

{-


writeAsMIDI :: FilePath -> EXT.StdPart1 anno -> IO ()
writeAsMIDI path ph = 
    let notes   = MIDI.translateToMidiP $ transExternalToIRBeam ph
        trk     = MIDI.translateToMIDI (MIDI.simpleTrackData 1) notes
    in MIDI.writeMF1 path [trk]
-}

--------------------------------------------------------------------------------
-- Debug...

outputAsTabular :: (Pretty pch, Pretty drn) 
                => ScoreInfo -> EXT.Part pch drn anno -> String
outputAsTabular _gi ph = ppRender $ mainTabular lo ph
  where
    lo = LeafOutputNote { pp_pitch     = pPrint
                        , pp_duration  = pPrint
                        , pp_anno      = const empty
                        }

printAsTabular :: (Pretty pch, Pretty drn) 
               => ScoreInfo -> EXT.Part pch drn anno ->  IO ()
printAsTabular gi = putStrLn . outputAsTabular gi


outputAsLinear :: (Pretty pch, Pretty drn) 
               => ScoreInfo -> EXT.Part pch drn anno -> String
outputAsLinear _gi ph = ppRender $ mainLinear lo ph
  where
    lo = LeafOutputNote { pp_pitch     = pPrint
                        , pp_duration  = pPrint
                        , pp_anno      = const empty
                        }

printAsLinear :: (Pretty pch, Pretty drn) 
              => ScoreInfo -> EXT.Part pch drn anno ->  IO ()
printAsLinear gi = putStrLn . outputAsLinear gi


