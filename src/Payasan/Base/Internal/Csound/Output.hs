{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Csound.Output
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generate notelists from IStmts.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Csound.Output
  ( 

    ColumnSpecs
  , ColumnFormat(..)

  , csoundOutput
  , columnSpecs
  , defaultFormat
  , intFormat
  ) where


import Payasan.Base.Internal.Base
import Payasan.Base.Internal.Csound.IStmt

import Text.PrettyPrint.HughesPJ hiding ( empty, render ) -- package: pretty
import qualified Text.PrettyPrint.HughesPJ as P


import qualified Data.IntMap as IM
import Data.List ( sortBy, groupBy, mapAccumL )
import Numeric




type ColumnSpecs = IM.IntMap [ColumnFormat]

data ColumnFormat = ColumnFormat 
    { column_name       :: !String
    , column_width      :: !Int
    , column_precision  :: !Int
    }
  deriving (Show)




-- List of stmts does not need to be in order...

csoundOutput :: ColumnSpecs -> [IStmt] -> Doc
csoundOutput specs xs = 
    let gs      = groupBy sameInst $ sortBy compareIStmt xs
        docs    = map (printGroup specs) gs
    in vcrush docs

printGroup :: ColumnSpecs -> [IStmt] -> Doc
printGroup _     []     = P.empty
printGroup specs (i:is) = 
        columnHeaders (inst_num i) specs 
    $+$ initialEvent specs i 
    $+$ restEvents specs i is


columnHeaders :: Int -> ColumnSpecs -> Doc
columnHeaders i im = maybe P.empty fn $ IM.lookup i im
  where
    prefix = text ";ins" <+> rightPadString 7 "st" <+> rightPadString 7 "drn"
    fn xs = prefix <+> hsep (map columnHeader1 xs) 



initialEvent :: ColumnSpecs -> IStmt -> Doc
initialEvent specs (IStmt i st drn vs) = 
    let valuesP = getValuesPrinter i specs
        valsD   = hsep $ zipWith ($) valuesP vs
    in ppInst i <+> ppDuration st <+> ppDuration drn <+> valsD
  
    
restEvents :: ColumnSpecs -> IStmt -> [IStmt] -> Doc
restEvents specs initial is = vcrush $ snd $ mapAccumL fn initial is
  where
    fn old new@(IStmt i st drn vs) = 
        let valuesP = getValuesDiffPrinter i specs (ivalues old)
            valsD   = hsep $ zipWith ($) valuesP vs
            doc     = ppInst i <+> ppDuration st <+> ppDuration drn <+> valsD
        in (new,doc)
          

vcrush :: [Doc] -> Doc
vcrush = foldr ($+$) P.empty


ppInst :: Int -> Doc
ppInst i = rightPadString 4 ('i': show i)


-- | print with 3 dec places
ppDuration :: Decimal -> Doc
ppDuration = rightPadDecimal 7 3 



--------------------------------------------------------------------------------



columnSpecs :: [(Int,[ColumnFormat])] -> ColumnSpecs
columnSpecs = IM.fromList


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


columnHeader1 :: ColumnFormat -> Doc
columnHeader1 fmt = rightPadString (columnInterior fmt) (column_name fmt)


getValuesPrinter :: Int -> ColumnSpecs -> [(Value -> Doc)]
getValuesPrinter i im = go $ IM.findWithDefault [] i im 
  where
    go [] = repeat (ppValue (defaultFormat ""))
    go (x:xs) = ppValue x : go xs


-- | Length of ColumnSpecs and Values should match...
-- 
getValuesDiffPrinter :: Int -> ColumnSpecs -> [Value] -> [(Value -> Doc)]
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

