{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Backend.Csound.Internal.BeamToCsound
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Beam syntax to Csound i-stmts.
-- 
--------------------------------------------------------------------------------

module Payasan.Backend.Csound.Internal.BeamToCsound
  ( 

    GenIStmt
  , makeGenIStmt
  , translateToCsound

  ) where

import Payasan.Backend.Csound.Internal.IStmt
import Payasan.Backend.Csound.Internal.Syntax

import Payasan.Base.Internal.RewriteMonad
import Payasan.Repr.IRBeam.Syntax
import Payasan.Repr.IRBeam.TiedNoteStream

import Payasan.Base.Duration



newtype GenIStmt anno = GenIStmt { 
   gen :: CpsPitch -> anno -> Seconds -> Seconds -> IStmt }


makeGenIStmt :: (CpsPitch -> anno -> Seconds -> Seconds -> IStmt) -> GenIStmt anno
makeGenIStmt = GenIStmt


type Mon a = Rewrite Seconds a


translateToCsound :: GenIStmt anno -> Part CpsPitch Duration anno -> [IStmt]
translateToCsound gf ph = evalRewrite (partT gf ph) 0


-- Work in seconds rather than MIDI ticks at this stage.
-- It will be easier with seconds to extend with quantization
-- (swing).



advanceOnset :: Seconds -> Mon ()
advanceOnset d = puts (\s -> s+d)

onset :: Mon Seconds
onset = get

partT :: GenIStmt anno -> Part CpsPitch Duration anno -> Mon [IStmt]
partT gf ph = concat <$> mapM (elementT gf) (makeTiedNoteStream ph)
 


-- Ties have been coalesced at this point...
--
elementT :: GenIStmt anno -> Element CpsPitch Seconds anno -> Mon [IStmt]
elementT gf (NoteElem e a _)    = (\x -> [x]) <$> noteT gf a e

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


noteT :: GenIStmt anno -> anno -> Note CpsPitch Seconds -> Mon IStmt
noteT gf anno (Note pch drn)            = 
    do { ot <- onset
       ; advanceOnset drn
       ; let fn = gen gf 
       ; return $ fn pch anno ot drn
       }


