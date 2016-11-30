{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Analysis.Trace
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- 
--
--------------------------------------------------------------------------------

module Payasan.Score.Analysis.Trace
  (

    TracePart(..)
  , TraceBar(..)
  , TraceElement(..)

  ) where



import Data.Data


data TracePart a = TracePart { trace_bars :: [TraceBar a] }
  deriving (Data,Eq,Show,Typeable)


data TraceBar a = TraceBar { trace_elements :: [TraceElement a] }
  deriving (Data,Eq,Show,Typeable)


-- Usually Blank will be a rest, spacer etc.
data TraceElement a = Blank
                    | Element a
  deriving (Data,Eq,Show,Typeable)
