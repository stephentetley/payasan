{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.MIDI.BeamToMIDI
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Beam syntax to MIDI syntax.
-- 
--------------------------------------------------------------------------------

module Payasan.Base.Internal.MIDI.BeamToMIDI
  ( 

    translateToMIDI

  ) where

import qualified Payasan.Base.Internal.MIDI.PrimitiveSyntax     as T

import Payasan.Base.Internal.Base
import Payasan.Base.Internal.BeamSyntax
import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration

import Data.Ratio

-- | Translate should operate on: 
--
-- > Phrase T.MidiPitch Duration
-- 
-- Rather than:
--
-- > Phrase T.Pitch Duration
-- 
-- So we can handle MIDI drums
--


type Mon a = Rewrite Seconds a


translateToMIDI :: T.TrackData -> Phrase T.MidiPitch RDuration anno -> T.Track
translateToMIDI td ph = T.Track $ evalRewrite (phraseT td ph) 0


-- Work in seconds rather than MIDI ticks at this stage.
-- It will be easier with seconds to extend with quantization
-- (swing).



advanceOnset :: Seconds -> Mon ()
advanceOnset d = puts (\s -> s+d)

onset :: Mon Seconds
onset = get

phraseT :: T.TrackData -> Phrase T.MidiPitch RDuration anno -> Mon T.InterimTrack
phraseT td ph = 
    (\ns -> T.InterimTrack { T.track_config = td
                           , T.track_notes  = concat ns
                           })
        <$> mapM elementT (makeTiedNoteStream ph)
 


-- Ties have been coalesced at this point...
--
elementT :: Element T.MidiPitch Seconds anno -> Mon [T.MidiNote]
elementT (NoteElem e _ _)       = (\x -> [x]) <$> noteT e

elementT (Rest d)               = 
    do { advanceOnset d
       ; return []
       }

-- MIDI: Spacer is same as Rest
elementT (Spacer d)             = 
    do { advanceOnset d
       ; return []
       }

-- MIDI: Skip is same as Rest
elementT (Skip d)               = 
    do { advanceOnset d
       ; return []
       }

elementT (Chord ps d _ _)       = 
    do { ot <- onset
       ; advanceOnset d
       ; return $ map (makeNote ot d) ps
       }

elementT (Graces {})            = return []

elementT (Punctuation {})       = return []


noteT :: Note T.MidiPitch Seconds -> Mon T.MidiNote
noteT (Note pch drn)            = 
    do { ot <- onset
       ; advanceOnset drn
       ; return $ makeNote ot drn pch
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


noteDuration :: BPM -> RDuration -> Seconds
noteDuration bpm d = realToFrac d * (4 * quarterNoteDuration bpm)

quarterNoteDuration :: BPM -> Seconds
quarterNoteDuration bpm = realToFrac $ 60 / bpm

--------------------------------------------------------------------------------
-- Coalesce tied notes and chords

-- First step linearize and turn duration to seconds
-- need to be in lear form to concat tied notes chords across 
-- bar lines / note groups


makeTiedNoteStream :: Phrase T.MidiPitch RDuration anno -> [Element T.MidiPitch Seconds anno]
makeTiedNoteStream = coalesce . linearize


linearize :: Phrase T.MidiPitch RDuration anno -> [Element T.MidiPitch Seconds anno]
linearize (Phrase bs) = concatMap linearizeB bs

linearizeB :: Bar T.MidiPitch RDuration anno -> [Element T.MidiPitch Seconds anno]
linearizeB (Bar info cs) = 
    let bpm = local_bpm info in concatMap (linearizeNG bpm) cs


linearizeNG :: BPM -> NoteGroup T.MidiPitch RDuration anno -> [Element T.MidiPitch Seconds anno]
linearizeNG bpm (Atom e)            = [linearizeE bpm e]
linearizeNG bpm (Beamed es)         = concatMap (linearizeNG bpm) es
linearizeNG bpm (Tuplet spec es)    = map (scaleD (t%n)) $ concatMap (linearizeNG bpm) es
  where
    (TupletSpec t n _) = spec

linearizeE :: BPM -> Element T.MidiPitch RDuration anno -> Element T.MidiPitch Seconds anno
linearizeE bpm (NoteElem e a t)     = NoteElem (linearizeN bpm e) a t
linearizeE bpm (Rest d)             = Rest $ noteDuration bpm d
linearizeE bpm (Spacer d)           = Spacer $ noteDuration bpm d
linearizeE bpm (Skip d)             = Skip $ noteDuration bpm d
linearizeE bpm (Chord ps d a t)     = Chord ps (noteDuration bpm d) a t
linearizeE bpm (Graces ns)          = Graces $ map (linearizeN bpm) ns
linearizeE _   (Punctuation s)      = Punctuation s


linearizeN :: BPM -> Note T.MidiPitch RDuration -> Note T.MidiPitch Seconds
linearizeN bpm (Note pch drn)   = Note pch $ noteDuration bpm drn

-- Simplistic scaling of Tuplets - does this really work?
--
scaleD :: Ratio Int -> Element pch Seconds anno -> Element pch Seconds anno
scaleD sc elt = step (realToFrac sc) elt
  where
    step x (NoteElem n a t)     = NoteElem (note x n) a t
    step x (Rest d)             = Rest $ x * d
    step x (Spacer d)           = Spacer $ x * d
    step x (Skip d)             = Skip $ x * d
    step x (Chord ps d a t)     = Chord ps (x * d) a t
    step x (Graces ns)          = Graces $ map (note x) ns
    step _ (Punctuation s)      = Punctuation s

    note x (Note p d)           = Note p (x * d)


coalesce :: [Element T.MidiPitch Seconds anno] -> [Element T.MidiPitch Seconds anno]
coalesce []     = []
coalesce (x:xs) = step x xs
  where
    step a []     = [a]
    step a (b:bs) = case together a b of
                      Nothing -> a : step b bs
                      Just t -> step t bs

-- Join together notes or chords if tied (and have the same notes).
--
together :: Element T.MidiPitch Seconds anno 
         -> Element T.MidiPitch Seconds anno 
         -> Maybe (Element T.MidiPitch Seconds anno)
together (NoteElem n1 _ t1)     (NoteElem n2 a t2)    = 
    case together1 n1 n2 t1 of
      Just note -> Just $ NoteElem note a t2
      Nothing -> Nothing

together (Chord ps1 d1 _ TIE)   (Chord ps2 d2 a t)    = 
    if ps1 == ps2 then Just $ Chord ps2 (d1+d2) a t
                  else Nothing

together _                      _                     = Nothing



-- Together for notes...
--
together1 :: Note T.MidiPitch Seconds 
          -> Note T.MidiPitch Seconds 
          -> Tie 
          -> Maybe (Note T.MidiPitch Seconds)
together1 (Note p1 d1) (Note p2 d2) t 
    | p1 == p2 && t == TIE   = Just $ Note p1 (d1+d2)
    | otherwise              = Nothing

