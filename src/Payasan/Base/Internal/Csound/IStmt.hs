{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Csound.IStmt
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- i-stmt types.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Csound.IStmt
  ( 
    IStmt(..)
  , IStmtList
  , Value(..)
  , TimeSpan(..)

  , Seconds             -- re-export

  , timeSpanIStmt
  , ellipsisEq
  , sameInst
  , compareIStmt
  ) where


import Payasan.Base.Internal.Base
import Payasan.Base.Internal.Utils


import Data.Fixed
import Data.Function ( on )

-- Just encode i-stmts, f-stmts can be added during rendering.

data IStmt = IStmt 
    { inst_num      :: Int
    , istart        :: Seconds
    , iduration     :: Seconds
    , ivalues       :: [Value]
    }
  deriving (Eq,Show)


-- | Time span - note does not support monoid instance.
--
data TimeSpan = TimeSpan 
    { tspan_start       :: !Seconds
    , tspan_duration    :: !Seconds
    } 
  deriving (Eq,Show)

-- We want statements at a concrete type that allows snocing.
--
type IStmtList = H IStmt


-- | High level value with booleans (i.e. Higher level than Csound 
-- which has no booleans).
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





timeSpanIStmt :: IStmt -> TimeSpan
timeSpanIStmt stmt = TimeSpan (istart stmt) (iduration stmt)



-- | Don\'t ellide negative values or Strings...

ellipsisEq :: Value -> Value -> Bool
ellipsisEq (VInt i)   (VInt j)    | i >= 0 = i == j
ellipsisEq (VFloat i) (VFloat j)  | i >= 0 = i == j
ellipsisEq _          _                    = False



sameInst :: IStmt -> IStmt -> Bool
sameInst = on (==) inst_num



-- | Sort by instrument, then start time.
compareIStmt :: IStmt -> IStmt -> Ordering
compareIStmt i1 i2 = case (compare `on` inst_num) i1 i2 of     
    EQ -> (compare `on` istart) i1 i2
    c  -> c



--------------------------------------------------------------------------------








