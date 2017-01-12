{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.MIDI.BeamToMIDI
-- Copyright   :  (c) Stephen Tetley 2015-2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Beam syntax to MIDI syntax.
-- 
--------------------------------------------------------------------------------

module Payasan.PSC.MIDI.BeamToMIDI
  ( 

    translateToMIDI

  ) where

import qualified Payasan.PSC.MIDI.PrimitiveSyntax       as T
import qualified Payasan.Base.AltPitch                  as T

import Payasan.PSC.Base.RewriteMonad
import Payasan.PSC.Repr.External.Syntax
import Payasan.PSC.Repr.External.TiedNoteStream

import Payasan.Base.Basis
import Payasan.Base.Duration


-- | Translate should operate on: 
--
-- > Part T.MidiPitch Duration
-- 
-- Rather than:
--
-- > Part T.Pitch Duration
-- 
-- So we can handle MIDI drums
--


type Mon a = Rewrite () Seconds a


fromRight :: Either z a -> a
fromRight _ = error "fromRight MIDI bad"

translateToMIDI :: T.TrackData -> Part T.MidiPitch Duration anno -> T.Track
translateToMIDI td ph = T.Track $ fromRight $ evalRewrite (partT td ph) () 0


-- Work in seconds rather than MIDI ticks at this stage.
-- It will be easier with seconds to extend with quantization
-- (swing).



advanceOnset :: Seconds -> Mon ()
advanceOnset d = modify (\s -> s+d)

onset :: Mon Seconds
onset = get

partT :: T.TrackData -> Part T.MidiPitch Duration anno -> Mon T.InterimTrack
partT td ph = 
    (\ns -> T.InterimTrack { T.track_config = td
                           , T.track_notes  = concat ns
                           })
        <$> mapM elementT (makeTiedNoteStream ph)
 


-- Ties have been coalesced at this point...
--
elementT :: Element T.MidiPitch Seconds anno -> Mon [T.MidiNote]
elementT (Note p d _ _)         = (\x -> [x]) <$> noteT p d

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


noteT :: T.MidiPitch -> Seconds -> Mon T.MidiNote
noteT pch drn                   = 
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


