{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.LilyPond.OutTrans
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Convert Bracket syntax to LilyPond prior to printing.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.LilyPond.OutTrans
  (
    translate
  ) where



import qualified Payasan.Base.Internal.LilyPond.Syntax as T
import Payasan.Base.Internal.LilyPond.Utils

import Payasan.Base.Internal.BracketSyntax
import Payasan.Base.Internal.Utils

import Payasan.Base.Duration
import Payasan.Base.Pitch



translate :: GlobalRenderInfo -> Phrase Pitch -> T.LyPhrase
translate info ph = evalTrans (phraseT ph) info state_zero
  where
    -- The first duration should never match then we always start
    -- printing
    state_zero = St { previous_duration = dQuarter
                    , previous_pitch    = pitch_zero
                    }
    -- If AbsPitch then /previous pitch/ will never be used
    pitch_zero = case global_pitch_directive info of
                    RelPitch pch -> pch
                    AbsPitch -> middle_c



--------------------------------------------------------------------------------
-- Implementation



data St = St { previous_duration   :: Duration 
             , previous_pitch      :: Pitch
             }

type Mon a = Trans GlobalRenderInfo St a


previousDuration :: Mon Duration
previousDuration = gets previous_duration

setPrevDuration :: Duration -> Mon ()
setPrevDuration d = puts (\s -> s { previous_duration = d })

previousPitch :: Mon (Maybe Pitch)
previousPitch = fn <$> asks global_pitch_directive <*> gets previous_pitch
  where
    fn (AbsPitch) _    = Nothing
    fn (RelPitch {}) p = Just p

setPrevPitch :: Pitch -> Mon ()
setPrevPitch p = puts (\s -> s { previous_pitch = p })


phraseT :: Phrase Pitch -> Mon T.LyPhrase
phraseT (Phrase bs)             = T.LyPhrase <$> mapM barT bs



barT :: Bar Pitch -> Mon T.Bar
barT (Bar info cs)              = 
    do { css <- mapM ctxElementT cs
       ; return $ T.Bar info (concat css)
       }


-- | Remember - a beamed CtxElement may generate 1+ elements
--
ctxElementT :: CtxElement Pitch -> Mon [T.CtxElement]
ctxElementT (Atom e)            = (wrapL . T.Atom) <$> elementT e

ctxElementT (Tuplet spec cs)    = 
    (wrapL . T.Tuplet spec . concat) <$> mapM ctxElementT cs

ctxElementT (Beamed cs)         = concat <$> mapM ctxElementT cs



elementT :: Element Pitch  -> Mon T.Element
elementT (NoteElem a)           = T.NoteElem <$> noteT a
elementT (Rest d)               = T.Rest   <$> durationT d
elementT (Chord ps d)           = T.Chord  <$> mapM pitchT ps <*> durationT d
elementT (Graces ns)            = T.Graces <$> mapM noteT ns


noteT :: Note Pitch -> Mon T.Note
noteT (Note pch drn)          = T.Note <$> pitchT pch <*> durationT drn



-- No previous pitch indicates Absolute pitch mode
pitchT :: Pitch -> Mon T.Pitch
pitchT p1 = 
    do { opt <- previousPitch
       ; case opt of 
           Nothing -> return $ fromPitchAbs p1
           Just tp0 -> do { let tp1 = fromPitchRel p1 tp0
                          ; setPrevPitch p1
                          ; return tp1
                          }
       }



durationT :: Duration -> Mon T.NoteLength
durationT d1 =
   do { d0 <- previousDuration 
      ; if d1 == d0 
          then return T.DrnDefault
          else setPrevDuration d1 >> return (T.DrnExplicit d1)
      }



--------------------------------------------------------------------------------
-- Helpers

wrapL :: a -> [a]
wrapL a = [a]

