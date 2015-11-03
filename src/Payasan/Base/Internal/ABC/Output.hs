{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.ABC.Output
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Output ABC.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.ABC.Output
  ( 
    abcOutput
  ) where

import Payasan.Base.Internal.ABC.Syntax
import Payasan.Base.Internal.ABC.Utils
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Text.PrettyPrint.HughesPJ hiding ( Mode )       -- package: pretty


type CatOp = Doc -> Doc -> Doc

-- Generating output should be stateful so we can insert a 
-- newline every four lines.

type Mon a = Rewrite State a

data State = State { bar_column :: !Int
                   , prev_info  :: !LocalContextInfo
                   }

stateZero :: LocalContextInfo -> State
stateZero info = State { bar_column = 0
                       , prev_info  = info }

lineLen :: Mon Int
lineLen = gets bar_column

resetLineLen :: Mon ()
resetLineLen = puts (\s -> s { bar_column = 0 })

incrLineLen :: Mon ()
incrLineLen = puts (\s -> s { bar_column = 1 + bar_column s })


setInfo :: LocalContextInfo -> Mon () 
setInfo info = puts (\s -> s { prev_info = info })


deltaMetrical :: LocalContextInfo -> Mon (Maybe (Meter,UnitNoteLength))
deltaMetrical (LocalContextInfo { local_meter = m1
                               , local_unit_note_len = u1 }) = 
    fn <$> gets prev_info
  where
    fn prev 
        | local_meter prev == m1 && local_unit_note_len prev == u1 = Nothing
        | otherwise        = Just (m1,u1)

deltaKey :: LocalContextInfo -> Mon (Maybe Key)
deltaKey (LocalContextInfo { local_key = k1 }) = 
    fn <$> gets prev_info
  where
    fn prev 
        | local_key prev == k1 = Nothing
        | otherwise            = Just k1


--------------------------------------------------------------------------------


abcOutput :: ScoreInfo -> StaffInfo -> ABCPhrase1 anno -> Doc
abcOutput infos staff ph = header $+$ body
  where
    first_info  = maybe default_local_info id $ firstContextInfo ph
    header      = oHeader infos staff first_info
    body        = evalRewrite (oABCPhrase ph) (stateZero first_info)

-- | Note X field must be first K field should be last -
-- see abcplus manual page 11.
--
oHeader :: ScoreInfo -> StaffInfo -> LocalContextInfo -> Doc
oHeader infos staff locals = 
        field 'X' (int 1)
    $+$ field 'T' (text   $ score_title infos)
    $+$ field 'M' (meter  $ local_meter locals)
    $+$ field 'L' (unitNoteLength $ local_unit_note_len locals)
    $+$ field 'K' key_clef 
  where
    key_clef = (key $ local_key locals) <+> (clef $ staff_clef staff)

oABCPhrase :: ABCPhrase1 anno -> Mon Doc
oABCPhrase (Phrase [])          = return empty
oABCPhrase (Phrase (x:xs))      = do { d <- oBar x; step d xs }
  where
    step d []     = return $ d <+> text "|]"
    step d (b:bs) = do { i <- lineLen
                       ; if i > 4 then resetLineLen else incrLineLen
                       ; d1 <- oBar b 
                       ; let ac = if i > 4 then (d <+> char '|' $+$ d1) 
                                           else (d <+> char '|' <+> d1)
                       ; step ac bs
                       }


oBar :: ABCBar1 anno -> Mon Doc
oBar (Bar info cs)              = 
    do { dkey    <- deltaKey info
       ; dmeter  <- deltaMetrical info
       ; let ans = oNoteGroupList (<+>) cs
       ; setInfo info
       ; return $ prefixM dmeter $ prefixK dkey $ ans
       }
  where
    prefixK Nothing       = (empty <>)
    prefixK (Just k)      = (midtuneField 'K' (key k) <+>)
    prefixM Nothing       = (empty <>)
    prefixM (Just (m,u))  = let doc = ( midtuneField 'M' (meter m) 
                                       <> midtuneField 'L' (unitNoteLength u))
                            in (doc <+>)

oNoteGroupList :: CatOp -> [ABCNoteGroup1 anno] -> Doc
oNoteGroupList op xs            = sepList op $ map (oNoteGroup op) xs

oNoteGroup :: CatOp -> ABCNoteGroup1 anno -> Doc
oNoteGroup op (Atom e)          = oElement op e
oNoteGroup _  (Beamed cs)       = oNoteGroupList (<>) cs
oNoteGroup op (Tuplet spec cs)  = tupletSpec spec <> oNoteGroupList op cs


-- | Punctuation is not used by ABC.
--
-- Skip is treated as a spacer.
--
oElement :: CatOp -> ABCElement1 anno -> Doc
oElement op (NoteElem n _ t)    = tied op (note n) t
oElement _  (Rest d)            = rest d 
oElement _  (Spacer d)          = spacer d 
oElement _  (Skip d)            = spacer d 
oElement op (Chord ps d _ t)    = tied op (chord ps d) t
oElement _  (Graces xs)         = graceForm $ map note xs
oElement _  (Punctuation {})    = empty


tied :: CatOp -> Doc -> Tie -> Doc
tied _  d NO_TIE = d
tied op d TIE    = d `op` char '-'