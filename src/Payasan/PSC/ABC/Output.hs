{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.ABC.Output
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output ABC.
--
--------------------------------------------------------------------------------

module Payasan.PSC.ABC.Output
  ( 
    
    ABCHeader
  , ABCNoteList

  , makeABCHeader
  , makeABCNoteList

  , assembleABC
  
  ) where

import Payasan.PSC.ABC.Base
import qualified Payasan.PSC.ABC.IRSimpleDoc as DOC
import Payasan.PSC.ABC.Pretty

import Payasan.PSC.Repr.External.Syntax

import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils


import Text.PrettyPrint.HughesPJ hiding ( Mode )       -- package: pretty


type CatOp = Doc -> Doc -> Doc





--------------------------------------------------------------------------------

-- ABC can handle annotations - it simply ignores them.


-- Doc fragments use a common (phantom) type.
-- This prevents a proliferation of slightly differently 
-- named wrappers / unwrappers, but allow some type 
-- differentiation (safety) in APIs.
--


data ABCHeader_ 

type ABCHeader = TyDoc ABCHeader_ 





-- | "Document assembly" is so simple that it does not need to 
-- run in a monad.
--
assembleABC :: ABCHeader -> ABCNoteList -> Doc
assembleABC header body = extractDoc header $+$ extractDoc body



-- makeABCHeader is wrong - don't expose SectionInfo


    
-- | Note X field must be first K field should be last -
-- see abcplus manual page 11.
-- 
-- At some point we should allow this function to be customized...
--
makeABCHeader :: String -> Clef -> SectionInfo -> ABCHeader
makeABCHeader title clefname locals = 
    TyDoc $ vcat $
       [ field 'X' (int 1)
       , field 'T' (text title)
       , field 'M' (meter  $ section_meter locals)
       , field 'L' (unitNoteLength $ section_unit_note_len locals)
       , field 'K' key_clef 
       ]
  where
    key_clef = (key $ section_key locals) <+> (clef clefname)


--
-- ABC demands X field is first in header.
-- ABC recommends T field is second in header
-- ABC demands K is last field in header (ABCPlus manual p11)
-- ABC recommends M field before L
--
-- Midtune fields are printed in square brackets, each midtune 
-- field is printed in its own set of square brackets.
--


makeABCNoteList :: Int -> Part ABCPitch ABCNoteLength anno -> ABCNoteList
makeABCNoteList cols part = renderDocPart cols $ toIRSimpleDoc part


toIRSimpleDoc :: Part ABCPitch ABCNoteLength anno -> DOC.Part a
toIRSimpleDoc part@(Part { part_sections = ss}) = 
    DOC.Part { DOC.part_sections = zipWith sectionT prefixes ss }
  where
    prefixes = midtuneFields part

    sectionT prefix (Section { section_name = name
                             , section_bars = bars }) = 
            DOC.Section { DOC.section_name   = name
                        , DOC.section_header = prefix
                        , DOC.section_bars   = map barT bars }
    
    barT bar = DOC.Bar { DOC.bar_content = renderBar bar }


-- TODO assembleABC provides the wrong interface, when it is 
-- improved use headerSuffix...

headerSuffix :: Part pch drn anno -> Doc
headerSuffix (Part { part_sections = ss }) = case ss of
    [] -> empty_header
    (s1:_) -> let info = section_info s1
                  d1   = field 'M' (meter $ section_meter info)
                  d2   = field 'L' (unitNoteLength $ section_unit_note_len info)
                  d3   = field 'K' (key $ section_key info)
              in d1 $+$ d2 $+$ d3


-- | Just 'K' = c major
--
empty_header :: Doc
empty_header = field 'K' (key c_maj)


-- | The first (Maybe Doc) is always Nothing - this is because
-- midtune fields represent deltas / changes to the initial 
-- context given in the header.
--
-- A Nothing represents no changes / no midtune fields to print.
--
midtuneFields :: Part pch drn anno -> [Maybe Doc]
midtuneFields part = case sectionInfos part of
    [] -> []
    (s:ss) -> Nothing : step s ss
  where
    step _ []       = []
    step i (s:ss)   = let d1 = meterChange i s
                          d2 = unitNoteLenChange i s
                          d3 = keyChange i s
                      in aggregateMaybes (<+>) [d1,d2,d3] : step s ss


keyChange :: SectionInfo -> SectionInfo -> Maybe Doc
keyChange (SectionInfo { section_key = old }) 
          (SectionInfo { section_key = new }) = 
    if new /= old then Just $ midtuneField 'K' (key new) 
                  else Nothing

meterChange :: SectionInfo -> SectionInfo -> Maybe Doc
meterChange (SectionInfo { section_meter = old }) 
            (SectionInfo { section_meter = new }) = 
    if new /= old then Just $ midtuneField 'M' (meter new) 
                  else Nothing

unitNoteLenChange :: SectionInfo -> SectionInfo -> Maybe Doc
unitNoteLenChange (SectionInfo { section_unit_note_len = old }) 
                  (SectionInfo { section_unit_note_len = new }) = 
    if new /= old then Just $ midtuneField 'L' (unitNoteLength new) 
                  else Nothing



renderBar :: Bar ABCPitch ABCNoteLength anno -> Doc
renderBar (Bar { note_groups = ns }) = 
    let op = (<+>) in noteGroupList op ns
  where
    noteGroupList op xs             = sepList op $ map (noteGroup op) xs
    
    noteGroup op (Atom e)           = element op e
    noteGroup _  (Beamed cs)        = noteGroupList (<>) cs
    noteGroup op (Tuplet spec cs)   = tupletSpec spec <> noteGroupList op cs

    element op (Note p d _ t)       = tied op (note p d) t
    element _  (Rest d)             = rest d 
    element _  (Spacer d)           = spacer d 
    element _  (Skip d)             = spacer d 
    element op (Chord ps d _ t)     = tied op (chord ps d) t
    element _  (Graces xs)          = graceForm $ map (\(Grace1 p d) -> note p d) xs
    element _  (Punctuation {})     = empty


-- | For the time being sections start on a new staff...
--
renderDocPart :: Int -> DOC.Part a -> ABCNoteList
renderDocPart cols (DOC.Part { DOC.part_sections = ss }) = 
    TyDoc $ sectionsD ss
  where
    sectionsD []     = empty
    sectionsD [d]    = sectionD (text "|]") d
    sectionsD (d:ds) = sectionD (text "|") d $+$ sectionsD ds

    sectionD end (DOC.Section { DOC.section_header = mb_prefix 
                              , DOC.section_bars   = dbars }) = 
          let dbars1 = map DOC.bar_content dbars 
              body   = ppSection cols end dbars1
          in case mb_prefix of
                Nothing -> body
                Just doc -> doc <+> body



tied :: CatOp -> Doc -> Tie -> Doc
tied _  d NO_TIE = d
tied op d TIE    = d `op` char '-'






-- Allows different terminator:
--
-- '|' for end of section, "|]" for end of part.
--
ppSection :: Int -> Doc -> [Doc] -> Doc
ppSection cols end bars = 
    ppTable cols (<+>) $ punctuateSepEnd (char '|') end bars
    


