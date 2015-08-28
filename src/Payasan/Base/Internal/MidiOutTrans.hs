{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.MidiOutTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Main syntax to MIDI syntax.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.MidiOutTrans
  ( 
    translate
  ) where


import Payasan.Base.Internal.Base
import Payasan.Base.Internal.MainSyntax
import qualified Payasan.Base.Internal.MidiOutput as T
import qualified Payasan.Base.Internal.MidiSyntax as T

import Payasan.Base.Duration
-- import Payasan.Base.Pitch

-- | Translate should operate on: 
--
-- > Phrase T.MidiPitch Duration
-- 
-- Rather than:
--
-- > Phrase T.MidiPitch Duration
-- 
-- So we can handle MIDI drums
--

translate :: T.TrackData -> Phrase T.MidiPitch Duration -> T.Track
translate td ph = T.render $ evalMon (phraseT td ph) 0


-- Work in seconds rather than MIDI ticks at this stage.
-- It will be easier with seconds to extend with quantization
-- (swing).

type St = Seconds

newtype Mon a = Mon { getMon :: St -> (St, a) }

instance Functor Mon where
  fmap f ma = Mon $ \s -> let (s1,a) = getMon ma s in (s1, f a)

instance Applicative Mon where
  pure a    = Mon $ \s -> (s,a)
  mf <*> ma = Mon $ \s -> let (s1,f) = getMon mf s
                              (s2,a) = getMon ma s1
                          in (s2, f a)

instance Monad Mon where
  return    = pure
  ma >>= k  = Mon $ \s -> let (s1,a) = getMon ma s 
                          in getMon (k a) s1

evalMon :: Mon a -> St -> a
evalMon ma s = let (_,a) = getMon ma s in a


advanceOnset :: Seconds -> Mon ()
advanceOnset d = Mon $ \s -> (s+d, ())

onset :: Mon Seconds
onset = Mon $ \s -> (s,s)

phraseT :: T.TrackData -> Phrase T.MidiPitch Duration -> Mon T.InterimTrack
phraseT td (Phrase bs) = 
    (\nss -> T.InterimTrack { T.track_config = td
                            , T.track_notes  = concat nss
                            })
        <$> mapM barT bs
 


barT :: Bar T.MidiPitch Duration -> Mon [T.MidiNote]
barT (Bar info cs)           = concat <$> mapM (ctxElementT df) cs
  where
    df = noteDuration (render_bpm info)
     


ctxElementT :: (Duration -> Seconds) -> CtxElement T.MidiPitch Duration -> Mon [T.MidiNote]
ctxElementT df (Atom e)         = elementT df e
ctxElementT _  (Tuplet {})      = return []

elementT :: (Duration -> Seconds) 
         -> Element T.MidiPitch Duration -> Mon [T.MidiNote]
elementT df (NoteElem a)        = (\x -> [x]) <$> noteT df a

elementT df (Rest d)            = 
    do { let d1 = df d
       ; advanceOnset d1
       ; return []
       }

elementT df (Chord ps d)        = 
    do { ot <- onset
       ; let d1 = df d
       ; advanceOnset d1
       ; return $ map (makeNote ot d1) ps
       }

elementT _  (Graces {})         = return []


noteT :: (Duration -> Seconds) -> Note T.MidiPitch Duration -> Mon T.MidiNote
noteT df (Note pch drn)        = 
    do { ot <- onset
       ; let d1 = df drn
       ; advanceOnset d1
       ; return $ makeNote ot d1 pch
       }



-- TODO should have some individual control over velocities.
--
makeNote :: Seconds -> Seconds -> T.MidiPitch -> T.MidiNote
makeNote ot d p = T.MidiNote 
    { T.note_start    = ot
    , T.note_dur      = d
    , T.note_value    = T.NoteValue { T.note_pitch = p
                                    , T.note_velo_on  = 127
                                    , T.note_velo_off = 0
                                    }
    }


noteDuration :: BPM -> Duration -> Seconds
noteDuration bpm d = realToFrac (durationSize d) * (4 * quarterNoteDuration bpm)

quarterNoteDuration :: BPM -> Seconds
quarterNoteDuration bpm = realToFrac $ 60 / bpm

