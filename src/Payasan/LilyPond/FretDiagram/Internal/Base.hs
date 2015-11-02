{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.LilyPond.FretDiagram.Internal.Base
-- Copyright   :  (c Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Fret diagrams for LilyPond.
--
--------------------------------------------------------------------------------

module Payasan.LilyPond.FretDiagram.Internal.Base
  ( 

    FretDiagramPhrase
  , FretDiagram(..)
  , Fingering(..)
  , BarreIndicator(..)
  , StringNumber
  , FretNumber(..)

  , pushName

  , GuitarTuning
  , standard_tuning

  , diagramDU

  ) where

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.LilyPond.Utils
import Payasan.Base.Monophonic.Internal.Syntax (Phrase)

import Payasan.Base.Duration
import Payasan.Base.Pitch
import Payasan.Base.Names.Pitch

import Text.PrettyPrint.HughesPJClass           -- package: pretty

import Data.Data
import Data.List (sortBy)


type FretDiagramPhrase = Phrase [Pitch] Duration FretDiagram


data FretDiagram = FretDiagram
     { fd_name           :: !String
     , fd_opt_barre      :: Maybe BarreIndicator
     , fd_fingerings     :: [Fingering]
     }
  deriving (Data,Eq,Show,Typeable)



data Fingering = Fingering  
    { fingering_string      :: !StringNumber   
    , fingering_fret        :: !FretNumber
    }
  deriving (Data,Eq,Show,Typeable)
 
data BarreIndicator = BarreIndicator
    { barre_low_string          :: !Int    -- 6 low (musically), 1 high
    , barre_high_string         :: !Int
    , barre_fret_position       :: !Int
    }
  deriving (Data,Eq,Show,Typeable)


type StringNumber = Int

data FretNumber = OPEN | MUTED | FretNumber !Int
  deriving (Data,Eq,Show,Typeable)



pushName :: String -> FretDiagram -> FretDiagram
pushName ss a = a { fd_name  = ss }


-- Sorted list - low to high strings
type GuitarTuning = [Pitch]


standard_tuning :: GuitarTuning
standard_tuning = [ e_3, a_3, d_4, g_4, b_4, e_5 ]

descSort :: [Fingering] -> [Fingering]
descSort = sortBy fn
  where
    -- reverses (should not have equal matches or more than one barre)
    fn a b | fingering_string a < fingering_string b   = GT
           | otherwise                                 = LT

instance Pretty FretDiagram where
  pPrint (FretDiagram { fd_opt_barre   = barre
                      , fd_fingerings  = xs }) = 
      command "fret-diagram" <+> char '#' <> doubleQuotes body
    where
      strings = cat $ map pPrint $ descSort xs
      body    = case barre of { Just b -> pPrint b <> strings
                              ; Nothing -> strings }
      
 

instance Pretty Fingering where
  pPrint (Fingering { fingering_string = s
                    , fingering_fret   = n }) = 
      int s <> char '-' <> pPrint n <> char ';'

instance Pretty BarreIndicator where
  pPrint (BarreIndicator { barre_low_string    = lo
                         , barre_high_string   = hi
                         , barre_fret_position = fret }) =
        text "c:" <> dashSep [ int lo, int hi, int fret ] <> char ';'
    
  

instance Pretty FretNumber where
  pPrint (OPEN)         = char 'o'  
  pPrint (MUTED)        = char 'x'
  pPrint (FretNumber i) = int i


dashSep :: [Doc] -> Doc
dashSep []      = empty
dashSep [d]     = d
dashSep (d:ds)  = d <> char '-' <> dashSep ds

-- | Note - the @universe@ of defs is not closed.
--
-- There are as many defs as there are diagrams defined.
--
diagramDU :: [FretDiagram] -> AnnoDU FretDiagram
diagramDU fs = AnnoDU { defs  = vcat $ map diagramDef fs
                      , use   = diagramUse
                      }

diagramDef :: FretDiagram -> Doc
diagramDef fd@(FretDiagram { fd_name = s }) = 
    text s <+> char '=' <+> block (Just $ command "markup") (pPrint fd)

diagramUse :: FretDiagram -> Doc
diagramUse (FretDiagram { fd_name = s }) = char '^' <> command s


