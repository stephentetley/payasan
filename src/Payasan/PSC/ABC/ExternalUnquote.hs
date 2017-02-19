{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.ABC.ExternalUnquote
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert ABC to Main Syntax, plus /pushing/ render info
-- into bars as this cannot be done during parsing / 
-- quasiquoting.
--
--------------------------------------------------------------------------------

module Payasan.PSC.ABC.ExternalUnquote
  (
    unquoteABC
  ) where

import Payasan.PSC.ABC.Common

import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.Traversals

import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Scale


type PTMon a = Mon () a
type DTMon a = Mon UnitNoteLength a


unquoteABC :: String -> SectionInfo -> ABCSectionQuote -> Section Pitch Duration ()
unquoteABC name info (ABCSectionQuote bs) =
    let bars = translateDuration info $ translatePitch info bs
    in Section { section_name      = name
               , section_info      = info
               , section_bars      = bars
               }

--------------------------------------------------------------------------------
-- Pitch translation

translatePitch :: SectionInfo 
               -> [Bar ABCPitch drn anno] 
               -> [Bar Pitch drn anno]
translatePitch info = genTransformBars elementP info () 

   

elementP :: Element ABCPitch drn anno -> PTMon (Element Pitch drn anno)
elementP (Note p d a t)         = (\p1 -> Note p1 d a t)  <$> transPch p
elementP (Rest d)               = pure $ Rest d
elementP (Spacer d)             = pure $ Spacer d
elementP (Skip d)               = pure $ Skip d
elementP (Chord ps d a t)       = 
    (\ps1 -> Chord ps1 d a t) <$> mapM transPch ps

elementP (Graces ns)            = Graces <$> mapM grace1P ns
elementP (Punctuation s)        = pure $ Punctuation s


grace1P :: Grace1 ABCPitch drn -> PTMon (Grace1 Pitch drn)
grace1P (Grace1 p d)            = (\p1 -> Grace1 p1 d) <$> transPch p



-- Pitches might be /natural/ in the score when the are
-- actually sharpened or flattened according to key 
-- signature

transPch :: ABCPitch -> PTMon Pitch
transPch p0 = (\k -> toPitch (buildScale k) p0) <$> asks section_key



--------------------------------------------------------------------------------
-- Translate duration


translateDuration :: SectionInfo 
                  -> [Bar pch ABCNoteLength anno] 
                  -> [Bar pch Duration anno]
translateDuration info = 
    genTransformBars elementD info (section_unit_note_len info)


elementD :: Element pch ABCNoteLength anno -> DTMon (Element pch Duration anno)
elementD (Note p d a t)         = (\d1 -> Note p d1 a t) <$> changeDrn d
elementD (Rest d)               = Rest      <$> changeDrn d
elementD (Spacer d)             = Spacer    <$> changeDrn d
elementD (Skip d)               = Skip      <$> changeDrn d
elementD (Chord ps d a t)       = (\d1 -> Chord ps d1 a t) <$> changeDrn d
elementD (Graces ns)            = Graces    <$> mapM grace1D ns
elementD (Punctuation s)        = pure $ Punctuation s


grace1D :: Grace1 pch ABCNoteLength -> DTMon (Grace1 pch Duration)
grace1D (Grace1 p d)            = Grace1 p <$> changeDrn d


changeDrn :: ABCNoteLength -> DTMon Duration
changeDrn d                     = 
    (\unl -> toDuration unl d) <$> asks section_unit_note_len


