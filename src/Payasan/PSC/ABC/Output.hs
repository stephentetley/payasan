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
  , makeHeader
  , makeABCNoteListDoc
  , assembleABC
  
  ) where

import Payasan.PSC.ABC.Utils

import Payasan.PSC.Repr.External.Syntax

import Payasan.PSC.Base.ABCCommon
import Payasan.PSC.Base.SyntaxCommon
import Payasan.PSC.Base.Utils

import Payasan.Base.Basis
import Payasan.Base.Scale

import Text.PrettyPrint.HughesPJ hiding ( Mode )       -- package: pretty

import Control.Monad.State

type CatOp = Doc -> Doc -> Doc


type Mon a = State SectionInfo a





deltaMetrical :: SectionInfo -> Mon (Maybe (Meter,UnitNoteLength))
deltaMetrical (SectionInfo { section_meter = m1
                           , section_unit_note_len = u1 }) = 
    fn <$> get
  where
    fn prev 
        | section_meter prev == m1 && section_unit_note_len prev == u1 = Nothing
        | otherwise        = Just (m1,u1)


deltaKey :: SectionInfo -> Mon (Maybe Key)
deltaKey (SectionInfo { section_key = k1 }) = 
    fn <$> get
  where
    fn prev 
        | section_key prev == k1 = Nothing
        | otherwise           = Just k1


--------------------------------------------------------------------------------

-- ABC can handle annotations - it simply ignores them.


type ABCPartOut anno            = Part        ABCPitch ABCNoteLength anno
type ABCSectionOut anno         = Section     ABCPitch ABCNoteLength anno
type ABCBarOut anno             = Bar         ABCPitch ABCNoteLength anno
type ABCNoteGroupOut anno       = NoteGroup   ABCPitch ABCNoteLength anno
type ABCElementOut anno         = Element     ABCPitch ABCNoteLength anno


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
assembleABC :: ABCHeader -> ABCNoteListDoc -> Doc
assembleABC header body = extractDoc header $+$ extractDoc body

    
-- | Note X field must be first K field should be last -
-- see abcplus manual page 11.
-- 
-- At some point we should allow this function to be customized...
--
makeHeader :: String -> Clef -> SectionInfo -> ABCHeader
makeHeader title clefname locals = 
    TyDoc $ vcat $
       [ field 'X' (int 1)
       , field 'T' (text title)
       , field 'M' (meter  $ section_meter locals)
       , field 'L' (unitNoteLength $ section_unit_note_len locals)
       , field 'K' key_clef 
       ]
  where
    key_clef = (key $ section_key locals) <+> (clef clefname)


-- | Note that line count for ABC output is "sketchy" - we 
-- might expect users to hand-edit formatting for final printing 
-- if bars do not fit nicely in 3s or 4s (no need to add 
-- advanced capabilities to PSC).
--
makeABCNoteListDoc :: Int -> SectionInfo -> ABCPartOut anno -> ABCNoteListDoc
makeABCNoteListDoc cols info (Part xs) = 
    evalState (TyDoc <$> step xs) info
  where
    step []       = return empty
    step [s]      = oSection cols (text "|]") s
    step (s:ss)   = do { d1 <- oSection cols (char '|') s
                       ; ds <- step ss
                       ; return (d1 $+$ ds)
                       }

-- | Midtune fields are printed in square brackets so they don't
-- need a separator (is this true?).
-- 
-- TODO - midtune fields need improving for clarity.
--
oSection :: Int -> Doc -> ABCSectionOut anno -> Mon Doc
oSection cols end (Section _ info cs) = 
    do { dkey    <- deltaKey info
       ; dmeter  <- deltaMetrical info
       ; let ans = ppSection cols end $ map oBar cs 
       ; put info
       ; return $ prefixM dmeter $ prefixK dkey $ ans
       }
  where
    prefixK Nothing       = (empty <>)
    prefixK (Just k)      = (midtuneField 'K' (key k) <+>)
    prefixM Nothing       = (empty <>)
    prefixM (Just (m,u))  = let doc = ( midtuneField 'M' (meter m) 
                                       <> midtuneField 'L' (unitNoteLength u))
                            in (doc <+>)

oBar :: ABCBarOut anno -> Doc
oBar (Bar cs) = oNoteGroupList (<+>) cs

oNoteGroupList :: CatOp -> [ABCNoteGroupOut anno] -> Doc
oNoteGroupList op xs            = sepList op $ map (oNoteGroup op) xs

oNoteGroup :: CatOp -> ABCNoteGroupOut anno -> Doc
oNoteGroup op (Atom e)          = oElement op e
oNoteGroup _  (Beamed cs)       = oNoteGroupList (<>) cs
oNoteGroup op (Tuplet spec cs)  = tupletSpec spec <> oNoteGroupList op cs


-- | Punctuation is not used by ABC.
--
-- Skip is treated as a spacer.
--
oElement :: CatOp -> ABCElementOut anno -> Doc
oElement op (Note p d _ t)      = tied op (note p d) t
oElement _  (Rest d)            = rest d 
oElement _  (Spacer d)          = spacer d 
oElement _  (Skip d)            = spacer d 
oElement op (Chord ps d _ t)    = tied op (chord ps d) t
oElement _  (Graces xs)         = graceForm $ map (\(Grace1 p d) -> note p d) xs
oElement _  (Punctuation {})    = empty


tied :: CatOp -> Doc -> Tie -> Doc
tied _  d NO_TIE = d
tied op d TIE    = d `op` char '-'




-- (Somewhat) general combinator to pretty print sections.
-- It is general in the sense that everything has already been
-- turned into a Doc. Maybe we need a naming convention for this 
-- type of combinator.



-- Allows different terminator:
--
-- '|' for end of section, "|]" for end of part.
--
ppSection :: Int -> Doc -> [Doc] -> Doc
ppSection cols end bars = 
    ppTable cols (<+>) $ punctuateSepEnd (char '|') end bars
    
    