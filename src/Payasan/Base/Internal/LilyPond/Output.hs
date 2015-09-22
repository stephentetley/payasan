{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.Output
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.Output
  ( 
    lilyPondOutput

  , LyOutputDef(..)
  , renderNotes
  ) where

import Payasan.Base.Internal.LilyPond.Syntax
import Payasan.Base.Internal.LilyPond.Utils
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Text.PrettyPrint.HughesPJ        -- package: pretty


type Mon a = Rewrite State a

data State = State { prev_info  :: !LocalRenderInfo }

stateZero :: LocalRenderInfo -> State
stateZero info = State { prev_info  = info }


setInfo :: LocalRenderInfo -> Mon () 
setInfo info = puts (\s -> s { prev_info = info })


deltaMetrical :: LocalRenderInfo -> Mon (Maybe Meter)
deltaMetrical (LocalRenderInfo { local_meter = m1 }) = 
    fn <$> gets prev_info
  where
    fn prev 
        | local_meter prev == m1 = Nothing
        | otherwise              = Just m1

deltaKey :: LocalRenderInfo -> Mon (Maybe Key)
deltaKey (LocalRenderInfo { local_key = k1 }) = 
    fn <$> gets prev_info
  where
    fn prev 
        | local_key prev == k1 = Nothing
        | otherwise            = Just k1


--------------------------------------------------------------------------------

lilyPondOutput :: GlobalRenderInfo 
               -> LyOutputDef Pitch anno 
               -> LyPhrase anno -> Doc
lilyPondOutput globals def ph = 
        header 
    $+$ block Nothing (modeBlockF $ (notes_header $+$ notes))
  where
    local1          = maybe default_local_info id $ firstRenderInfo ph
    header          = oHeader globals
    modeBlockF      = octaveModeBlock (global_ly_octave_mode globals)
    notes_header    = oPhraseHeader local1
    notes           = renderNotes def ph


oHeader :: GlobalRenderInfo -> Doc
oHeader globals = 
        version (global_ly_version globals)
    $+$ block (Just $ command "header") (title $ global_title globals)

oPhraseHeader :: LocalRenderInfo -> Doc
oPhraseHeader locals = 
        key   (local_key locals)
    $+$ meter (local_meter locals)

octaveModeBlock :: OctaveMode -> Doc -> Doc
octaveModeBlock (AbsPitch)   d  = absolute $+$ d
octaveModeBlock (RelPitch p) d  = block (Just $ relative p) d


--------------------------------------------------------------------------------
-- Notelist

-- Design note - we only want to write this once.
-- Should allow different pch (standard, drum note, etc.)
-- to be printed. 


data LyOutputDef pch anno = LyOutputDef 
    { printPitch    :: pch -> Doc
    , printAnno     :: anno -> Doc
    }

-- | Pitch should be \"context free\" at this point.
--
renderNotes :: forall pch anno. LyOutputDef pch anno -> GenLyPhrase pch anno -> Doc
renderNotes def ph = evalRewriteDefault (oLyPhrase ph) (stateZero first_info)
  where
    first_info :: LocalRenderInfo
    first_info  = maybe default_local_info id $ firstRenderInfo ph

    pPitch :: pch -> Doc
    pPitch = printPitch def

    pAnno  :: anno -> Doc
    pAnno  = printAnno def

    oLyPhrase :: GenLyPhrase pch anno -> Mon Doc
    oLyPhrase (Phrase [])           = return empty
    oLyPhrase (Phrase (x:xs))       = do { d <- oBar x; step d xs }
      where
        step d []     = return d
        step d (b:bs) = do { d1    <- oBar b
                           ; let ac = d <+> char '|' $+$ d1
                           ; step ac bs 
                           }

    oBar :: GenLyBar pch anno -> Mon Doc
    oBar (Bar info cs)              = 
          do { dkey    <- deltaKey info
             ; dmeter  <- deltaMetrical info
             ; let ans = hsep (map oNoteGroup cs)
             ; setInfo info
             ; return $ prefixM dmeter $ prefixK dkey $ ans
             }
        where
          prefixK Nothing   = (empty <>)
          prefixK (Just k)  = (key k $+$)
          prefixM Nothing   = (empty <>)
          prefixM (Just m)  = (meter m $+$)

    oNoteGroup :: GenLyNoteGroup pch anno -> Doc
    oNoteGroup (Atom e)             = oElement e
    oNoteGroup (Beamed cs)          = beamForm $ map oNoteGroup cs
    oNoteGroup (Tuplet spec cs)     = tupletSpec spec <+> hsep (map oNoteGroup cs)

    oElement :: GenLyElement pch anno -> Doc
    oElement (NoteElem n a)         = oNote n <> pAnno a
    oElement (Rest d)               = rest d 
    oElement (Chord ps d a)         = 
        chordForm (map pPitch ps) <> noteLength d <> pAnno a

    oElement (Graces ns)            = graceForm (map oNote ns)


    oNote :: GenLyNote pch -> Doc
    oNote (Note p d)               = pPitch p <> noteLength d