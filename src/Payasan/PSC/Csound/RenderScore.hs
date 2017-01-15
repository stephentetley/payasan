{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Csound.RenderScore
-- Copyright   :  (c) Stephen Tetley 2016-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Render IREventFlat to a Csound score.
-- 
-- At this stage the score could be embedded into a csd file or 
-- output as a separate sco file.
-- 
-- We aim for good printing hence events are grouped by 
-- instrument and (ideally) column headings are printed.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Csound.RenderScore
  ( 
    renderScore

  , defaultFormat
  , intFormat
  ) where


import Payasan.PSC.Csound.Base

import Payasan.PSC.Repr.IREventFlat.Syntax

import Payasan.Base.Basis


import Text.PrettyPrint.HughesPJ hiding ( empty, render ) -- package: pretty
import qualified Text.PrettyPrint.HughesPJ as P


import qualified Data.IntMap    as IM
import qualified Data.List      as List
import Numeric ( showFFloat )


-- NOT COMPILING - need to pass a dictionary to make Csound events.

{-
-- | Assume part is a single instrument.
makeCsdEventListDoc :: ColumnSpecs -> Part Seconds ScoEvent -> CsdEventListDoc
makeCsdEventListDoc specs p = 
-}

-- firstInst :: Part ot ScoEvent -> Maybe Int
-- firstInst (Part (e:_)) = Just $ inst_num $ event_body e
-- firstInst _            = Nothing


renderScore :: ColumnSpecs -> Part Seconds Seconds anno -> Doc 
renderScore specs p1 = error "OLD" -- renderPart1 specs p1
     
{-

-- At this point all events should be generated by the same 
-- instrument
--
renderPart1 :: ColumnSpecs -> Part Seconds ScoEvent -> Doc
renderPart1 specs (Part { part_sections = ss }) = 
    vcrush $ map (renderSection specs) ss
    -- columnHeaders (inst_num $ event_body e) specs 


renderSection :: ColumnSpecs -> Section Seconds ScoEvent -> Doc
renderSection specs (Section { section_events = es}) = step es
  where
    step []     = P.empty
    step (e:es) = initialEvent specs e $+$ restEvents specs e es



columnHeaders :: InstNumber -> ColumnSpecs -> Doc
columnHeaders inst im = maybe P.empty fn $ IM.lookup inst im
  where
    prefix = text ";ins" <+> rightPadString 7 "st" <+> rightPadString 7 "drn"
    fn xs = prefix <+> hsep (map columnHeader1 xs) 



initialEvent :: ColumnSpecs -> Event Seconds ScoEvent -> Doc
initialEvent specs (Event ot (ScoEvent inst drn vs)) = 
    let valuesP = getValuesPrinter inst specs
        valsD   = hsep $ zipWith ($) valuesP vs
    in ppInst inst <+> ppDuration ot <+> ppDuration drn <+> valsD
  
    
restEvents :: ColumnSpecs 
           -> Event Seconds ScoEvent 
           -> [Event Seconds ScoEvent] 
           -> Doc
restEvents specs initial is = vcrush $ snd $ List.mapAccumL fn initial is
  where
    fn old new@(Event ot (ScoEvent inst drn vs)) = 
        let valuesP = getValuesDiffPrinter inst specs (event_params $ event_body old)
            valsD   = hsep $ zipWith ($) valuesP vs
            doc     = ppInst inst <+> ppDuration ot <+> ppDuration drn <+> valsD
        in (new,doc)


vcrush :: [Doc] -> Doc
vcrush = foldr ($+$) P.empty


ppInst :: Int -> Doc
ppInst i = rightPadString 4 ('i': show i)


-- | print with 3 dec places
ppDuration :: Decimal -> Doc
ppDuration = rightPadDecimal 7 3 

-}

--------------------------------------------------------------------------------
-- This is old code - to be improved / removed when we have a
-- better way of handling value ellipses (probably add a new
-- constructor to Value, and do a find ellipses transformation
-- before rendering).




defaultFormat :: String -> ColumnFormat
defaultFormat name = ColumnFormat { column_name       = name
                                  , column_width      = 7
                                  , column_precision  = 3
                                  }

intFormat :: String -> Int -> ColumnFormat 
intFormat name width = ColumnFormat { column_name       = name
                                    , column_width      = width
                                    , column_precision  = 0
                                    }

{-

columnHeader1 :: ColumnFormat -> Doc
columnHeader1 fmt = rightPadString (columnInterior fmt) (column_name fmt)


getValuesPrinter :: InstNumber -> ColumnSpecs -> [(Value -> Doc)]
getValuesPrinter i im = go $ IM.findWithDefault [] i im 
  where
    go [] = repeat (ppValue (defaultFormat ""))
    go (x:xs) = ppValue x : go xs


-- | Length of ColumnSpecs and Values should match...
-- 
getValuesDiffPrinter :: InstNumber -> ColumnSpecs -> [Value] -> [(Value -> Doc)]
getValuesDiffPrinter i im olds = 
    zipWith fn olds $ mkInf (IM.findWithDefault [] i im)
  where
    fn old fmt = \v1 -> if old `ellipsisEq` v1 then ppEllision fmt 
                                               else ppValue fmt v1                     
    mkInf fmts = fmts ++ repeat (defaultFormat "")           
--
-- Note - The Csound book prints with left alignment. 
-- No attempt is made to prettify e.g. align to decimal point.
--

columnInterior :: ColumnFormat -> Int
columnInterior = (subtract 1) . column_width


ppValue :: ColumnFormat -> Value -> Doc
ppValue fmt (VBool b)           = 
    rightPadInt (columnInterior fmt) (if b then 1 else 0)

ppValue fmt (VStr s)            = rightPadString (columnInterior fmt) s

ppValue fmt (VInt i)            = rightPadInt (columnInterior fmt) i

ppValue fmt (VFloat d)          = 
    rightPadDecimal (columnInterior fmt) (column_precision fmt) (realToFrac d)

ppValue fmt (VCpsPitch d)       = 
    rightPadDecimal (columnInterior fmt) (column_precision fmt) (realToFrac d)



ppEllision :: ColumnFormat -> Doc
ppEllision fmt = rightPadString (columnInterior fmt) "."


--------------------------------------------------------------------------------
-- 



rightPadString :: Int -> String -> Doc
rightPadString colw ss = text $ ss ++ replicate (colw - length ss) ' '

-- | Print a decimal with the supplied precision
-- (first argument), right pad with spaces if the output is 
-- shorter than the column width (second arg).
--
rightPadDecimal :: Int -> Int -> Decimal -> Doc
rightPadDecimal colw prec d = 
    rightPadString colw $ ($ "") $ showFFloat (Just prec) d1
  where
    d1 :: Double
    d1 = realToFrac d


rightPadInt :: Int -> Int -> Doc
rightPadInt colw i = rightPadString colw $ show i


-- Note - this module implies changes to the organization of 
-- Payasan.PSC.Backend.Csound.Output as we want to support 
-- embedding into csd files, plus ColumnSpecs might be better
-- defined elsewhere.
-- 

-}