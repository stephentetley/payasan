{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Models.UTT.Base
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- ...
--
--------------------------------------------------------------------------------

module Payasan.Models.UTT.Base
  ( 
  -- * Triad and UTT data types
    Mode(..)
  , Triad(..)
  , UTT(..)


  , act
  , modePreserving
  , modeReversing

  -- * Transformations
  , transZero
  , transN
  , parallel
  , leittonweschel
  , relative
  , dominant
  , mediant

  -- Riemannian
  , total
  , riemannian
  , schritt
  , weschel

  , unitSchritt
  , skew

  ) where


import Payasan.Models.Base.Classes
import Payasan.Models.Base.Z12



-- Mode is Z2 so it could have Num, AdditiveGroup... instances

data Mode = Pos | Neg 
  deriving (Enum,Eq,Ord)


instance Show Mode where
  showsPrec _ Pos = showChar '+'
  showsPrec _ Neg = showChar '-'

instance Invert Mode where
  invert Pos = Neg
  invert Neg = Pos

instance Mult Mode where
  Pos ^*^ Pos = Pos
  Pos ^*^ Neg = Neg
  Neg ^*^ Pos = Neg
  Neg ^*^ Neg = Pos
  


-- Triad - root x mode
--
data Triad = Triad 
      { triad_root :: Z12
      , triad_mode :: Mode 
      }
  deriving (Eq,Ord)

instance Show Triad where
  showsPrec _ (Triad t rho) = parens (shows t . showChar ',' . shows rho)
    where
      parens f = showChar '(' . f . showChar ')'

-- | Uniform triadic transformation - sign x t-major x t-minor
--
data UTT = UTT 
      { utt_sign                  :: Mode 
      , major_transposition_level :: Z12
      , minor_transposition_level :: Z12 
      }
  deriving (Eq,Ord)

instance Show UTT where
  showsPrec _ (UTT s tpos tneg) = 
      angles (shows s . showChar ',' . shows tpos . showChar ',' . shows tneg)
    where
      angles f = showChar '<' . f . showChar '>'



select :: Mode -> UTT -> Z12
select Pos (UTT _ tpos _   ) = tpos
select Neg (UTT _ _    tneg) = tneg


instance Mult UTT where
  (UTT rhoU tposU tnegU)  ^*^ v@(UTT rhoV _ _) = UTT rho tpos tneg
    where
      rho  = rhoU ^*^ rhoV
      tpos = tposU + select rhoU v
      tneg = tnegU + select (invert rhoU) v


instance Invert UTT where
  invert u@(UTT rho _ _) = 
    UTT rho (invert $ select rho u) (invert $ select (invert rho) u)


act :: Triad -> UTT -> Triad
act (Triad t rhoD) u@(UTT rhoU _ _) = Triad (t + select rhoD u) (rhoD ^*^ rhoU) 

modePreserving :: UTT -> Bool
modePreserving (UTT rho _ _) = rho == Pos

modeReversing :: UTT -> Bool
modeReversing (UTT rho _ _)  = rho == Neg

--------------------------------------------------------------------------------

transZero       :: UTT
transZero       = UTT Pos 0 0

transN          :: Z12 -> UTT
transN n        = UTT Pos n n 

parallel        :: UTT
parallel        = UTT Neg 0 0

leittonweschel  :: UTT
leittonweschel  = UTT Neg 4 8

relative        :: UTT
relative        = UTT Neg 9 3

dominant        :: UTT
dominant        = UTT Pos 5 5 

mediant         :: UTT
mediant         = UTT Neg 9 8


--------------------------------------------------------------------------------

total           :: UTT -> Z12
total (UTT _ tpos tneg) = tpos + tneg 

riemannian :: UTT -> Bool
riemannian u    = total u == 0

schritt         :: Z12 -> UTT
schritt n       = UTT Pos n (negate n)

weschel         :: Z12 -> UTT
weschel n       = UTT Neg n (negate n)


unitSchritt     :: UTT
unitSchritt     = schritt 1

skew            :: UTT -> Z12
skew (UTT rho tpos tneg) | rho == Pos   = tpos + tneg
                         | otherwise    = tpos + tneg + 6

