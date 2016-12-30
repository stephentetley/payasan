{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Csound.Base
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Common objects and helpers to generate good Csound scores
--
--------------------------------------------------------------------------------

module Payasan.PSC.Csound.Base
  ( 


    InstNumber
  , ColumnSpecs
  , ColumnFormat(..)
  , columnSpecs
  
  , ScoEvent(..)
  , sameInst
  
  , Value(..)
  , ellipsisEq

  ) where


import Payasan.PSC.Base.SyntaxCommon (TyDoc)
import Payasan.Base.Basis

import Text.PrettyPrint.HughesPJ ( Doc, text )  -- package: pretty


import Data.Fixed
import Data.Function (on)
import qualified Data.IntMap            as IM




--------------------------------------------------------------------------------
-- Column formatting

type InstNumber = Int

type ColumnSpecs = IM.IntMap [ColumnFormat]

data ColumnFormat = ColumnFormat 
    { column_name       :: !String
    , column_width      :: !Int
    , column_precision  :: !Int
    }
  deriving (Show)



columnSpecs :: [(InstNumber,[ColumnFormat])] -> ColumnSpecs
columnSpecs = IM.fromList


--------------------------------------------------------------------------------
-- Event

-- Lowest level Csound event - we know intrument number and 
-- duration, plus we have access to params so we can print them
-- but we don't know anything about what the params mean (so 
-- we can't transform them, e.g. increase amplitude, change 
-- pitch).
--
data ScoEvent = ScoEvent
    { inst_num          :: Int
    , event_duration    :: Seconds
    , event_params      :: [Value]
    }
  deriving (Eq,Show)
  

sameInst :: ScoEvent -> ScoEvent -> Bool
sameInst = on (==) inst_num


--------------------------------------------------------------------------------
-- Value

-- | Value with booleans - slightly higher level than Csound 
-- which has no booleans.
-- 
-- This represents the value in an i- or f-statement in a Csound 
-- score.
-- 
data Value = VStr       !String
           | VInt       !Int
           | VFloat     !Decimal
           | VBool      !Bool
           | VCpsPitch  !Milli
  deriving (Eq,Show)
  


-- REVISED THINKING 
-- Printing ellipses in scores is not a great feature
-- for the complexity it adds to PSC...

-- | Don\'t ellide negative values or Strings...

ellipsisEq :: Value -> Value -> Bool
ellipsisEq (VInt i)   (VInt j)    | i >= 0 = i == j
ellipsisEq (VFloat i) (VFloat j)  | i >= 0 = i == j
ellipsisEq _          _                    = False
  