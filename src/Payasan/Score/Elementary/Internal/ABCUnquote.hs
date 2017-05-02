{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.ABCUnquote
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert ABC to Elementary syntax.
--
--------------------------------------------------------------------------------

module Payasan.Score.Elementary.Internal.ABCUnquote
  (

    elementary_abc
  , unquoteABC

  ) where



import Payasan.Score.Elementary.Internal.ABCParser
import Payasan.Score.Elementary.Internal.Syntax
import Payasan.Score.Elementary.Internal.Traversals

import Payasan.PSC.ABC.Base hiding ( ABCSectionQuote(..) )
import Payasan.PSC.Base.SyntaxCommon

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Scale

import Language.Haskell.TH.Quote


import Control.Monad.Reader


--------------------------------------------------------------------------------
-- Quasiquote

-- Note - unfortunately we can\'t parameterize the quasiquoter
-- (e.g. with default note length)


elementary_abc :: QuasiQuoter
elementary_abc = QuasiQuoter
    { quoteExp = \s -> case parseABCSection s of
                         Left err -> error $ show err
                         Right xs -> dataToExpQ (const Nothing) xs
    , quoteType = \_ -> error "QQ - no Score Type"
    , quoteDec  = \_ -> error "QQ - no Score Decl"
    , quotePat  = \_ -> error "QQ - no Score Patt" 
    } 


type PMon a = Mon () a
type DMon a = Mon UnitNoteLength a

unquoteABC :: String -> SectionInfo -> ABCSectionQuote anno -> Section Pitch Duration anno
unquoteABC name info (ABCSectionQuote bs) =
    let bars = trafoDuration info $ trafoPitch info bs
    in Section { section_name   = name
               , section_info   = info
               , section_bars   = bars
               }

--------------------------------------------------------------------------------
-- Pitch translation

trafoPitch :: SectionInfo -> [Bar ABCPitch drn anno] -> [Bar Pitch drn anno]
trafoPitch info = genTransformBars elementP info ()


elementP :: Element ABCPitch drn anno -> PMon (Element Pitch drn anno) 
elementP (Note p d a t)         = (\p1 -> Note p1 d a t) <$> transPch p
elementP (Rest d)               = pure $ Rest d
elementP (Spacer d)             = pure $ Spacer d
elementP (Skip d)               = pure $ Skip d
elementP (Punctuation s)        = pure $ Punctuation s



transPch :: ABCPitch -> PMon Pitch
transPch p0 = 
    (\k -> toPitch (buildScale k) p0) <$> asks (sectionKeyWithDefault c_maj)

--------------------------------------------------------------------------------
-- Translate duration

trafoDuration :: SectionInfo ->  [Bar pch ABCNoteLength anno] -> [Bar pch Duration anno]
trafoDuration info = genTransformBars elementD info UNIT_NOTE_8


elementD :: Element pch ABCNoteLength anno -> DMon (Element pch Duration anno)
elementD (Note p d a t)         = (\d1 -> Note p d1 a t) <$> changeDuration d
elementD (Rest d)               = Rest    <$> changeDuration d
elementD (Spacer d)             = Spacer  <$> changeDuration d
elementD (Skip d)               = Skip    <$> changeDuration d
elementD (Punctuation s)        = pure $ Punctuation s


changeDuration :: ABCNoteLength -> DMon Duration
changeDuration d = (\unl -> toDuration unl d) <$> asks section_unit_note_len

