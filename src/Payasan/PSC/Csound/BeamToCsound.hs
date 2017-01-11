{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Csound.BeamToCsound
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert External syntax to Csound i-stmts.
--
-- This module is old and in the process of being superceded.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.Csound.BeamToCsound
  ( 

    GenIStmt
  , makeGenIStmt
  , translateToCsound

  ) where

import Payasan.PSC.Csound.IStmt hiding ( onset )
import Payasan.PSC.Csound.Syntax
import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.TiedNoteStream

import Payasan.PSC.Base.RewriteMonad

import Payasan.Base.Duration



newtype GenIStmt anno = GenIStmt { 
   gen :: CpsPitch -> anno -> Seconds -> Seconds -> IStmt }


makeGenIStmt :: (CpsPitch -> anno -> Seconds -> Seconds -> IStmt) -> GenIStmt anno
makeGenIStmt = GenIStmt


type Mon a = Rewrite () Seconds a


fromRight :: Either z a -> a
fromRight _ = error "fromRight awful"


translateToCsound :: GenIStmt anno -> Part CpsPitch Duration anno -> [IStmt]
translateToCsound gf ph = fromRight $ evalRewrite (partT gf ph) () 0


-- Work in seconds rather than MIDI ticks at this stage.
-- It will be easier with seconds to extend with quantization
-- (swing).



advanceOnset :: Seconds -> Mon ()
advanceOnset d = modify (\s -> s+d)

onset :: Mon Seconds
onset = get

partT :: GenIStmt anno -> Part CpsPitch Duration anno -> Mon [IStmt]
partT gf ph = concat <$> mapM (elementT gf) (makeTiedNoteStream ph)
 


-- Ties have been coalesced at this point...
--
elementT :: GenIStmt anno -> Element CpsPitch Seconds anno -> Mon [IStmt]
elementT gf (Note p d a _)    = (\x -> [x]) <$> noteT gf a p d

elementT _  (Rest d)            = 
    do { advanceOnset d
       ; return []
       }

-- MIDI: Spacer is same as Rest
elementT _  (Spacer d)          = 
    do { advanceOnset d
       ; return []
       }

-- MIDI: Skip is same as Rest
elementT _  (Skip d)            = 
    do { advanceOnset d
       ; return []
       }

elementT gf (Chord ps d a _)    = 
    do { ot <- onset
       ; advanceOnset d
       ; let fn = gen gf
       ; return $ map (\p -> fn p a ot d) ps
       }

elementT _  (Graces {})         = return []

elementT _  (Punctuation {})    = return []


noteT :: GenIStmt anno -> anno -> CpsPitch -> Seconds -> Mon IStmt
noteT gf anno pch drn           = 
    do { ot <- onset
       ; advanceOnset drn
       ; let fn = gen gf 
       ; return $ fn pch anno ot drn
       }


