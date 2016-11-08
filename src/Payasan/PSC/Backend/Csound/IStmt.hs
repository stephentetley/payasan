{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Backend.Csound.IStmt
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- i-stmt types.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Backend.Csound.IStmt
  ( 
    IStmt(..)
  , IStmtList
  , Value(..)

  , Seconds             -- re-export

  , ellipsisEq
  , sameInst
  , compareIStmt
  ) where


import Payasan.Base.Basis
import Payasan.Base.Utils


import Data.Fixed
import Data.Function ( on )




-- Just encode i-stmts, f-stmts can be added during rendering.
--
-- Design note - this is an intrinsic representation - start and duration
-- are inside the @event@ rather than wrapped listitem.
data IStmt = IStmt 
    { inst_num      :: Int
    , istart        :: Seconds
    , iduration     :: Seconds
    , ivalues       :: [Value]
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








