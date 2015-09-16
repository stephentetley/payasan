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

import Text.PrettyPrint.HughesPJ        -- package: pretty




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
renderNotes def = oLyPhrase
  where
    pPitch :: pch -> Doc
    pPitch = printPitch def

    pAnno  :: anno -> Doc
    pAnno  = printAnno def

    oLyPhrase :: GenLyPhrase pch anno -> Doc
    oLyPhrase (Phrase [])           = empty
    oLyPhrase (Phrase (x:xs))       = step (oBar x) xs
      where
        step d []     = d
        step d (b:bs) = let ac = d <+> char '|' $+$ oBar b in step ac bs

    oBar :: GenLyBar pch anno -> Doc
    oBar (Bar _info cs) = hsep (map oNoteGroup cs)

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