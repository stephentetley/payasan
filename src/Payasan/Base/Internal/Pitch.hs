{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Pitch
-- Copyright   :  (c) Stephen Tetley 2014-2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Pitch type and helpers - Z12 numeric type, MIDI pitch, 
--
-- Import directly if needed.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Pitch
  ( 

    Z12
  , Modulo12(..)


  , Pitch
  , makePitch
  , deconsPitch
  , flatten
  , sharpen

  , addSemitones

  , midiToPitch
  , pitchToMidi

  , MidiPitch
  , Interval

  , Chord(..)
  , ChordIntervals
  , chordNotes
  , chordF
  , chordAdd
  , chordDelete

  ) where



import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace


import Data.Data
import qualified Data.IntMap as IM



newtype Z12 = Z12 Int
  deriving (Data,Enum,Eq,Integral,Ord,Real,Typeable)

instance Show Z12 where
  showsPrec p (Z12 i) = showsPrec p i

instance Read Z12 where
  readsPrec p s       = map fn $ readsPrec p s
    where
      fn :: (Int,String) -> (Z12,String)
      fn (i,ss) = (Z12 i, ss)


liftUZ12 :: (Int -> Int) -> Z12 -> Z12
liftUZ12 op (Z12 a) = Z12 $ mod (op a) 12

liftBZ12 :: (Int -> Int -> Int) -> Z12 -> Z12 -> Z12
liftBZ12 op (Z12 a) (Z12 b) = Z12 $ mod (a `op` b) 12

instance Num Z12 where
  (+) = liftBZ12 (+)
  (-) = liftBZ12 (-)
  (*) = liftBZ12 (*)
  negate        = liftUZ12 negate
  fromInteger i = Z12 $ (fromInteger i) `mod` 12
  signum _      = error "Modular numbers are not signed"
  abs _         = error "Modular numbers are not signed"



instance AdditiveGroup Z12 where
  zeroV = 0
  (^+^) = (+)
  negateV v = 0 - v


--------------------------------------------------------------------------------
-- Pitch


-- Note - this representation avoids pitch (and interval) naming
-- as we are only generating numerical values. 
-- 
-- Recovering spelling would be good for ghci...
-- 


-- | Pitch-class representation - octave and pitch-class
--
-- The Csound book calls this @cpspch@.
--
-- Middle C is 8.00.
-- 
data Pitch = Pitch Int Z12
  deriving (Data,Eq,Ord,Typeable)

instance Show Pitch where
  showsPrec _ (Pitch o i) = shows o . showChar '.' . fn i
    where
      fn n | n < 10    = showChar '0' . shows n
           | otherwise = shows n 
 

flatten :: Pitch -> Pitch
flatten p = p .-^ 1

sharpen :: Pitch -> Pitch
sharpen p = p .+^ 1


makePitch :: Int -> Int -> Pitch
makePitch o i = Pitch o (toZ12 i)

deconsPitch :: Pitch -> (Int,Int)
deconsPitch (Pitch o i) = (o, fromIntegral i)

midiToPitch :: MidiPitch -> Pitch
midiToPitch mp = Pitch (o+3) (fromIntegral i)
  where
    (o,i) = fromIntegral mp `divMod` 12

pitchToMidi :: Pitch -> MidiPitch
pitchToMidi (Pitch o i) = fromIntegral $ fromIntegral i + ove
  where
    ove = 12 * (o-3)




pchDiff :: Pitch -> Pitch -> Interval
pchDiff a b =  pitchToMidi a .-. pitchToMidi b


-- Simplest addition (subtraction...) is to go via an 
-- integer representation aka midi.
--
semitoneAdd :: Pitch -> Interval -> Pitch
semitoneAdd pch iv = midiToPitch $ (.+^ iv) $ pitchToMidi pch


addSemitones :: Int -> Pitch -> Pitch 
addSemitones sc pch = semitoneAdd pch (fromIntegral sc)


instance AffineSpace Pitch where
  type Diff Pitch = Interval
  (.-.) = pchDiff
  (.+^) = semitoneAdd


--------------------------------------------------------------------------------

class Modulo12 a where
  fromZ12 :: Z12 -> a
  toZ12   :: a  -> Z12


instance Modulo12 Int where
  fromZ12 (Z12 i) = i
  toZ12 i = Z12 $ mod i 12

instance Modulo12 Integer where
  fromZ12 (Z12 i) = fromIntegral i
  toZ12 i         = Z12 $ fromIntegral $ mod i 12




--------------------------------------------------------------------------------
-- Midi Pitch



newtype MidiPitch = MidiPitch { getMidiPitch :: Int }
  deriving (Enum,Eq,Ord,Num,Real,Integral)

instance Show MidiPitch where
  showsPrec p d = showsPrec p (getMidiPitch d)


instance AffineSpace MidiPitch where
  type Diff MidiPitch = Interval
  (.-.) a b = Interval $ getMidiPitch a - getMidiPitch b
  (.+^) a b = MidiPitch $ getMidiPitch a + getInterval b




--------------------------------------------------------------------------------
-- Intervals


-- | Interval is just a count of a steps (semitones for ET12).
--
newtype Interval = Interval { getInterval :: Int }
  deriving (Enum,Eq,Ord,Integral,Num,Real)

instance Show Interval where
  showsPrec p d = showsPrec p (getInterval d)

instance AdditiveGroup Interval where
  zeroV   = 0
  negateV = negate
  (^+^)   = (+)


--------------------------------------------------------------------------------
-- Chords

type ChordIntervals = IM.IntMap Interval

data Chord = Chord 
    { chord_root   :: Pitch
    , chord_ivals  :: ChordIntervals 
    }
  deriving (Eq,Show)


-- No concat for chords, but they do support extension (adding 
-- more notes).


chordNotes :: Chord -> [Pitch]
chordNotes (Chord { chord_root = root
                  , chord_ivals = ivals }) = map (root .+^) $ IM.elems ivals


chordF :: (ChordIntervals -> ChordIntervals) -> Chord -> Chord
chordF f ch@(Chord { chord_ivals = ivals }) = ch { chord_ivals = f ivals}


chordAdd                :: Int -> Interval -> Chord -> Chord
chordAdd n inv          = chordF (IM.insert n inv)



chordDelete             :: Int -> Chord -> Chord
chordDelete n           = chordF (IM.delete n)

