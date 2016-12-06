{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Csound.IStmt
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- i-stmt types.
-- 
-- This module is old and in the process of being superceded.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Csound.IStmt
  ( 
    IStmt(..)
  , IStmtList

  , Seconds             -- re-export

  , ellipsisEq
  , sameInst
  , compareIStmt
  ) where

import Payasan.PSC.Csound.Base ( InstNumber, Value(..), ellipsisEq )
import Payasan.PSC.Base.Utils

import Payasan.Base.Basis


import Data.Function ( on )




-- Just encode i-stmts, f-stmts can be added during rendering.
--
-- Design note - this is an intrinsic representation - start and duration
-- are inside the @event@ rather than wrapped listitem.
data IStmt = IStmt 
    { inst_num      :: InstNumber
    , istart        :: Seconds
    , iduration     :: Seconds
    , ivalues       :: [Value]
    }
  deriving (Eq,Show)



-- We want statements at a concrete type that allows snocing.
--
type IStmtList = H IStmt



sameInst :: IStmt -> IStmt -> Bool
sameInst = on (==) inst_num



-- | Sort by instrument, then start time.
compareIStmt :: IStmt -> IStmt -> Ordering
compareIStmt i1 i2 = case (compare `on` inst_num) i1 i2 of     
    EQ -> (compare `on` istart) i1 i2
    c  -> c



--------------------------------------------------------------------------------








